"""
Python 2.7

Script to extract sentences of interest for the inference experiment:
  counterfactuals
  requests
  questions
  attitudinal and communicative verbs
  implicatives
"""

import argparse
import os
import re
import sys
from collections import defaultdict
from itertools import groupby

# Paths to local modules.
UPPEN_MORPH_PATH            = "resources/uppen_morph_analysis"
CUSTOMIZED_RESOURCE_PATH    = "resources/customized_resources"
REGEX_GEN_PATH              = "resources/regex_generation"

# Load local functions.
sys.path.append(UPPEN_MORPH_PATH)
from uppen_morph import load_morph_file, filter_morph_data 

sys.path.append(CUSTOMIZED_RESOURCE_PATH)
from customized_resources import load_stratos_impl_list, load_stratos_impl_forms

sys.path.append(REGEX_GEN_PATH)
from regex_gen import gen_regex



#
# Helper functions.
#

# Capitalizes a word or a list of words.
def capitalize(e):
  if type(e) == list:
    return [capitalize(w) for w in e]
  if type(e) == str:
    return e[0].upper() + e[1:]

# Parses arguments and returns the result of parser.parse_args() from argparse.
def parse_arguments():
  parser = argparse.ArgumentParser(
      description='Extracts sentences that may have phenomena of interest ' +
                  'from a dataset.')
  
  parser.add_argument('-stype', '--source_type', choices=['file', 'dir'], required=True)
  parser.add_argument('-spath', '--source_path', type=str, required=True)
  parser.add_argument('-tpath', '--target_path', type=str, required=True)
  parser.add_argument('-ipath', '--ignored_path', type=str)

  feature_parser = parser.add_mutually_exclusive_group(required=False)
  feature_parser.add_argument('-youknow', '--default_youknow', dest='ignore_youknow', action='store_false')
  feature_parser.add_argument('-iyouknow', '--ignore_youknow', dest='ignore_youknow', action='store_true')
  parser.set_defaults(ignore_youknow=False)

  # If switchboard match is true, then the matching phase is performed after
  # disfluency reductions.  However, the reported sentence is the original so
  # that these reductions can be done manually and more accurately.
  sw_parser = parser.add_mutually_exclusive_group(required=False)
  sw_parser.add_argument('-swmatch', '--switchboard_match', dest='switchboard_match', action='store_true')
  sw_parser.add_argument('-dfmatch', '--default_match', dest='switchboard_match', action='store_false')
  parser.set_defaults(switchboard_match=False)

  args = parser.parse_args()
  return args

# Forms a dictionary from token labels to token values for a line in the
# Switchboard corpus.
def line_token_map(line):
  tokens = line.split('\t')
  assert(len(tokens) == 7)
  tmap = { label : val for label, val in zip(SWITCHBOARD_TOKEN_LABELS, tokens) }
  return tmap

# Write the current utterance transcriptions (as stored in curid and curtmaps) 
# into lout and mout.
def write_line(curid, curtmaps, lout, mout):
  prefix = curid + "\t"
  suffix = "\n"
  if lout and curid:
    ltokens = [tmap['ldc_word'] for tmap in curtmaps]
    cleaned_ltokens = clean_tokens(ltokens)
    lout.write(prefix + " ".join(cleaned_ltokens) + suffix)
  if mout and curid:
    mtokens = [tmap['ms_word'] for tmap in curtmaps]
    cleaned_mtokens = clean_tokens(mtokens)
    mout.write(prefix + " ".join(cleaned_mtokens) + suffix)

def is_counterfactual(l):
  #for p in CF_RE:
  for p, schema in CF_SCHEMA_RE:
    if re.match(p, l):
      return schema
  return None

def is_request(l):
  #for p in REQUEST_RE:
  for p, schema in REQUEST_SCHEMA_RE:
    if re.match(p, l):
      return schema
  return None

def is_question(l):
  for p, schema in QUESTION_SCHEMA_RE:
    if re.match(p, l):
      return schema
  return None

def is_implicative(l):
  # TODO: generalize for various capitalizations
  # TODO: handle multi-token impl forms correctly.
  tokens = l.lower().split(" ")
  for impl_form in IMPL_FORMS:
    if impl_form in tokens:
      return impl_form
  return None


def is_interesting(l):
  return is_counterfactual(l) or is_request(l) or is_question(l) or \
      is_implicative(l)

# Return the merge of two dictionaries without modifying either input.
def merge_dicts(d1, d2):
  d = d1.copy()
  d.update(d2)
  return d

# Returns true if there's an occurrence of "know" not in "you know".
def is_raw_know(string):
  tokens = string.lower().split(" ")
  for i in range(len(tokens)):
    if tokens[i] == "know" and (i == 0 or tokens[i - 1] != "you"):
      return True
  return False

# This doesn't include ones like: so, like, well, etc. since these have
# fluent meaning.
DISFLUENCY_RE = [re.compile(p) for p in ["^uh$", "^\w+-$", "^-\w+$", "^um$", "^erm$"]]

# Reduces disfluencies from normalized sentence using a few simple rules:
#   1. Remove anything from a set of disfluencies : uh, -\w+, \w+-, 
#   2. Collapse repeated phrases up to length 3.
def reduce_disfluencies(l):
  tokens = l.split(" ")
  # Remove from list.
  filtered_tokens = [t for t in tokens if not any([re.match(p, t.lower()) for p in DISFLUENCY_RE])]
  # Collapse repeated phrases.
  final_tokens = filtered_tokens
  start = 0
  while start < len(final_tokens):
    diff = len(final_tokens) - start
    for width in range(diff, 0, -1):
      segment = [t.lower() for t in final_tokens[start:start + width]]
      reps = 0
      while segment == [t.lower() for t in final_tokens[start + (width * (reps + 1)):start + (width * (reps + 2))]]:
          reps += 1
      for i in range(reps * width):
        del final_tokens[start + width]
      if reps > 0:
        start += width - 1
        break
    start += 1
  return " ".join(final_tokens)


# Main function for sampling from files using patterns.
def filter_file(filename, out, iyouknow, swmatch):
  filtered = defaultdict(list)
  filter_counts = defaultdict(int)
  lines = file(filename, 'r').read().splitlines()
  ignored = []

  linenum = 0
  for l in lines:
    numberedl = (linenum, l)
    if linenum % 1000 == 0:
      print linenum
    matchl = l
    if swmatch:
      matchl = reduce_disfluencies(l)
    if is_interesting(matchl):
      filter_counts['interesting'] += 1
      if is_counterfactual(matchl):
        filtered['counterfactual'].append(numberedl)
        out.write(str(is_counterfactual(matchl)) + "[counterfactual]")
      if is_request(matchl):
        filtered['request'].append(numberedl)
        out.write(str(is_request(matchl)) + "[request]")
      if is_question(matchl):
        filtered['question'].append(numberedl)
        out.write(str(is_question(matchl)) + "[question]")
      if is_implicative(matchl):
        impl_match = str(is_implicative(matchl))
        if not iyouknow or impl_match != "know" or is_raw_know(l):
          # Only add if iyouknow is False or the match is not "know" or at
          # least one match of "know" is not preceded by "you"
          filtered['implicative'].append(numberedl)
          out.write(impl_match + "[implicative]")
      out.write(" --- ")
      out.write(l)
      out.write("\n")
    else:
      filter_counts['ignored'] += 1
      ignored.append(l)
    linenum += 1

  return (filtered, filter_counts, ignored)

def listdict_to_countdict(ld):
  return { k : len(v) for k, v in ld.iteritems() }

def countdict_update(cd1, cd2):
  for k, v in cd2.iteritems():
    cd1[k] +=  v
  return cd1

args = parse_arguments()

#
# Global vars.
#

WILL_FORMS = ["will", "would", "wo", "'d"]
CAN_FORMS = ["can", "could", "ca"]
WISH_FORMS = ["wish", "wished", "wishing"]
WH_Q_WORDS = ["what", "when", "where", "which", "who", "whom", "why", "how"]
AUX_VERBS = ["do", "does", "did", "has", "have", "is", "am", "are", "was", 
    "were", "be", "being", "been", "may", "must", "might", "should", "could", 
    "would", "shall", "will", "can", "need", "dare", "ca", "wo"]
PREPOSITIONS = ["with", "at", "from", "into", "during", "including", "until", "against", "among","throughout", "despite", "towards", "upon", "concerning", "of", "to", "in", "for", "on", "by", "about", "like", "through", "over", "before","between", "after", "since", "without", "under", "within", "along", "following", "across", "behind", "beyond", "plus", "except", "but", "up", "out", "around", "down", "off", "above", "near"]

# Non-subordinating sentence connecting words.
NSUB_SCONN_WORDS = ["and", "or"]
SCONN_PUNCT = [";", ":", "\"", "``", "'", "''"]

# Discourse markers that precede a statement.
DISCOURSE_PREFIX = ["well", "okay", "ok", "you know", "yknow", "y'know", "'know", 
    "of course", "course", "so", "right", "anyway", "like", "fine", "now", 
    "i mean", "good", "oh", "as i say", "as i said", "great", "mind you", 
    "for a start", "i see", "yes", "no", "yeah", "definitely", "wow", "wonderful", 
    "absolutely"]

REQUEST_MODALS = WILL_FORMS + CAN_FORMS
PERMISSION_MODALS = CAN_FORMS + ["may"]

# Load Stratos implicative list and get past tense forms.
STRATOS_IMPL = load_stratos_impl_list()
UPPEN_MORPHS = load_morph_file()
UPPEN_PAST  = [m.token for m in filter_morph_data(UPPEN_MORPHS, poses=["V"], features=["PAST"])]
UPPEN_PPART = [m.token for m in filter_morph_data(UPPEN_MORPHS, poses=["V"], features=["PPART"])]
UPPEN_PRES = list(set([li.lemma for m in \
    filter_morph_data(UPPEN_MORPHS, poses=["V"], features=[]) \
    for li in m.lemmas]))

UPPEN_VERBS = [m for m in filter_morph_data(UPPEN_MORPHS, poses=["V"], features=[])]
UPPEN_VERB_FORMS = list(set([m.token for m in UPPEN_VERBS] + \
    [li.lemma for m in UPPEN_VERBS for li in m.lemmas]))

CF_PATTERNS = [
    # Basic
    # TODO: add past verb and past participles.
    ".*(if|If) .*(was|were|had).*",
    ".*(if|If) .*(was|were|had).* (would|wouldn't|will|won't|'d) .*",
    # Inverted
    # TODO: add past verb and past participles.
    ".*(would|wouldn't|will|won't|'d).* (if|If) .*",
    # Implicit 'if'.
    "(Were|Had|were|had) .*",
    ".*(would|wouldn't|will|won't|'d) .* (were|had) .*",
    # 'wish'
    ".*(wish|Wish).*" # can add past verb/past participle to limit to counterfactuals.
    ]



REQUEST_PATTERNS = [
    # Basic.
    # TODO: Add variants with question marks that can be more general in the body. (for dataset with question marks we might as well take advantage of it)
    "(.+ |)(Would|would|Will|will|Can|can|Could|could) you .*",
    "(.+ |)(I|We|we|You|you) need you .+",
    ".* may I .*",
    "Could I .*",
    # Negative requests (aka "false requests")
    "(.+ |)(won't|wouldn't|can't|couldn't|Won't|Wouldn't|Can't|Couldn't) .+\?",
    "(.+ |)(wouldn't|couldn't|won't|can't|Wouldn't|Couldn't|Won't|Can't) you(| .+)",
    "(.+ |)(must|Must) you(| .+)"
    ]

#
# Define schema mappings.
#
GENERAL_SCHEMA_DEFS = {
  "<begin?>"    : "<regex>(.*\S+ |)",  # beginning of sentence or separated from previous word.
  "<end?>"      : "<regex>(| \S+.*)",  # end of sentence or separated from next word.
  "<mid?>"      : "<regex>( .+ | )",   # one space separation or spaces with something in the middle.
  "<mid>"       : "<regex>( .+ )",     # something in between
  "<1stpron>"   : ["i", "we"] + capitalize(["i", "we"])
    }

CF_SCHEMA_DEFS = merge_dicts(GENERAL_SCHEMA_DEFS,
    {
      # Move begin, end, mid to defaults.
      "<futr>"      : WILL_FORMS + capitalize(WILL_FORMS),
      "<wish>"      : WISH_FORMS + capitalize(WISH_FORMS),
      "<pres>"      : UPPEN_PRES,
      "<past>"      : UPPEN_PAST,
      "<ppart>"     : UPPEN_PPART
      })

REQUEST_SCHEMA_DEFS = merge_dicts(GENERAL_SCHEMA_DEFS,
    {
      "<reqmodal>"  : REQUEST_MODALS + capitalize(REQUEST_MODALS), 
      "<permmodal>" : PERMISSION_MODALS + capitalize(PERMISSION_MODALS),
      "<pres>"      : UPPEN_PRES
      })

QUESTION_SCHEMA_DEFS = merge_dicts(GENERAL_SCHEMA_DEFS,
    {
      "<aux>"   : AUX_VERBS + capitalize(AUX_VERBS),
      "<wh>"    : WH_Q_WORDS + capitalize(WH_Q_WORDS),
      "<verb>"  : UPPEN_VERB_FORMS,
      "<pred>"  : PREPOSITIONS + capitalize(PREPOSITIONS),
      # Connectors of sentences.
      "<sentconn>" : NSUB_SCONN_WORDS + capitalize(NSUB_SCONN_WORDS) + SCONN_PUNCT,
      # Discourse phenomena that precede the sentence.
      "<discpre>" : DISCOURSE_PREFIX + capitalize(DISCOURSE_PREFIX)
      })

#
# Declare schema patterns for each phenomenon.
#

CF_SCHEMAS = [
    # Basic
    "<begin?>(if|If)<mid>(was|were|had|<past>|<ppart>)<mid?>(<futr>) .+",
    # Inverted
    "<begin?>(<futr>)<mid>if<mid>(was|were|had|<past>|<ppart>) .+",
    # Implicit 'if'.
    "(Were|Had|were|had)<mid>(<futr>) .+",
    "<begin?>(<futr>)<mid>(were|had) .+",
    # 'wish'
    ".+ (<wish>) .+", # can add past verb/past participle to limit to counterfactuals.
    # More general one to allow negative examples.
    #"<begin?>(if|If)<mid>(was|were|had|<past>|<ppart>) .*", # fairly general explicit 'if'
    "<begin?>(if|If) .*", # most general explicit 'if'
    "(were|had|Were|Had)<end?>" # general implicit 'if'
    ]

# TODO: test further on a dataset with more requests (so far done on discourse treebank which came up with 2...)
REQUEST_SCHEMAS = [
    # Basic.
    # TODO: Add variants with question marks that can be more general in the body. (for dataset with question marks we might as well take advantage of it)
    # Can/Will you ...
    "<begin?>(<reqmodal>) you<mid?>(<pres>)<end?>",
    # I need you ... TODO: see if we should add "to" after "you"
    "<begin?>(<1stpron>) need you<mid?>(<pres>)<end?>",
    # Can/May I ... V[pres] ...
    "<begin?>(<permmodal>) I<mid?>(<pres>)<end?>",
    
    # Negative requests (aka "false requests")
    "<begin?>(<reqmodal>) (n't|not) you<mid?>(<pres>)<end?>",
    "<begin?>(must|Must) you<mid?>(<pres>)<end?>"
    ]

# TODO: Test with UIUC QC dataset to check coverage.
# TODO: Test with other datasets for false positive rate.
QUESTION_SCHEMAS = [
    # 1. All the patterns based on the beginnings of sentences need to allow
    #    happening after connectives, semicolons, colons, quotes.
    # 2. Same as 1, but followed by the connective.
    # Sentences starting with WH-words.
    "(^|<sentconn>|<discpre>)(<wh>)<end?>$",
    # Preposition followed by WH-word (can happen anywhere).
    "^<begin?>(<prep>) (<wh>)<end?>$",
    # Verb followed by a WH-word. NB: Too many false positives...
    #"^<begin?>(<verb>) (<wh>)<end?>$",
    # Ending in WH-word "You did this how/where?"
    "^<begin?>(<wh>)($|<sentconn>)",
    # Starting with an auxiliary e.g. "Shall we go"
    "(^|<sentconn>|<discpre>)(<aux>)<end?>$",
    # Sentences containing a question mark -- very strong signal for
    # well-formatted datasts and not likely to falsely match on datasets
    # without good punctuation.
    #"^<begin?>\?<end?>$",
    "^.*\?.*$"
    #"(^<begin?>\?|\?<end?>$|^.*\?.*$)"
    ]

#
# Generate regular expressions from schemas.
#

CF_RE = [re.compile(p) for p in CF_PATTERNS]
REQUEST_RE = [re.compile(p) for p in REQUEST_PATTERNS]

CF_SCHEMA_RE = [(re.compile(gen_regex(schema, CF_SCHEMA_DEFS, use_defaults=True)), schema) for \
    schema in CF_SCHEMAS]
REQUEST_SCHEMA_RE = [(re.compile(gen_regex(schema, REQUEST_SCHEMA_DEFS, use_defaults=True)), schema) for \
    schema in REQUEST_SCHEMAS]

QUESTION_SCHEMA_RE = [(re.compile(gen_regex(schema, QUESTION_SCHEMA_DEFS, use_defaults=True)), schema) for \
    schema in QUESTION_SCHEMAS]

# Get all forms of implicatives from the uppen morphological dataset.
UPPEN_VERBS = [m for m in filter_morph_data(UPPEN_MORPHS, poses=["V"], features=[])]
STRATOS_UPPEN = [m for m in UPPEN_VERBS if any([li.lemma in STRATOS_IMPL for li in m.lemmas])]
#IMPL_FORMS = list(set([m.token for m in STRATOS_UPPEN] + \
#    [li.lemma for m in STRATOS_UPPEN for li in m.lemmas]))

IMPL_FORMS = load_stratos_impl_forms()

# TODO: change the normalization scripts to include an index which this script can use to record which ones were sampled by index.

# Extract transcriptions.
if args.source_type == 'file':
  out = file(args.target_path, 'w')
  linemap, countmap, ignored = \
      filter_file(args.source_path, out, args.ignore_youknow, args.switchboard_match)
  out.close()  


  # TODO: factor code below to be more general
  #numsfortypes = {inftype: sorted(list(set([i for i, l in numvals]))) for inftype, numvals in linemap.iteritems()} 
  #typespernum = defaultdict(list)
  #for inftype, nums in numsfortypes.iteritems():
  #  for n in nums:
  #    typespernum[n].append(inftype)

  #for curtype, nums in numsfortypes.iteritems():
  #  onlycurnums = [n for n, types in typespernum.iteritems() if curtype in types and len(types) == 1]
  #  
  #  out = file("sampled_tatoeba_only_{}.nums".format(curtype), 'w')
  #  out.write("\n".join([str(e) for e in sorted(onlycurnums)]))
  #  out.close()
  #  out = file("sampled_tatoeba_all_{}.nums".format(curtype), 'w')
  #  out.write("\n".join([str(e) for e in sorted(nums)]))
  #  out.close()


  filter_counts = defaultdict(int)
  countdict_update(filter_counts, countmap)
  countdict_update(filter_counts, listdict_to_countdict(linemap))
  print filter_counts


else:
  numfile = 0
  filter_counts = defaultdict(int)
  all_ignored = []
  for dirname, subdirlist, filelist in os.walk(args.source_path):
    for f in filelist:
      # Only extract if it's an annotation file.
      print "Extracting file num {}: {}".format(numfile, f)

      # TODO: remove
      # Disfluency test.
      #out = file(os.path.join(args.target_path, f), 'w')
      #reduced = [reduce_disfluencies(l) for l in file(os.path.join(dirname, f), 'r').read().splitlines()] 
      #for r in reduced:
      #  out.write(r)
      #  out.write("\n")
      #out.close()


      numfile += 1
      out = file(os.path.join(args.target_path, f), 'w')
      linemap, countmap, ignored = \
          filter_file(os.path.join(dirname, f), out, args.ignore_youknow, args.switchboard_match)
      out.close()

      countdict_update(filter_counts, countmap)
      countdict_update(filter_counts, listdict_to_countdict(linemap))
      all_ignored.extend(ignored)
  if args.ignored_path:
    out = file(args.ignored_path, 'w')
    for l in all_ignored:
      out.write(l)
      out.write("\n")
    out.close()
  print filter_counts


