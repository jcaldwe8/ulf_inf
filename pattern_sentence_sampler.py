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
  # TODO
  return False

def is_implicative(l):
  # TODO: generalize for various capitalizations
  # TODO: handle multi-token impl forms correctly.
  tokens = l.split(" ")
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

def filter_file(filename, out):
  filtered = defaultdict(list)
  filter_counts = defaultdict(int)
  lines = file(filename, 'r').read().splitlines()

  for l in lines:
    if is_interesting(l):
      filter_counts['interesting'] += 1
      if is_counterfactual(l):
        filtered['counterfactual'].append(l)
        out.write(str(is_counterfactual(l)) + "[counterfactual]")
      if is_request(l):
        filtered['request'].append(l)
        out.write(str(is_request(l)) + "[request]")
      if is_question(l):
        filtered['question'].append(l)
        out.write(str(is_question(l)) + "[question]")
      if is_implicative(l):
        filtered['implicative'].append(l)
        out.write(str(is_implicative(l)) + "[implicative]")
      out.write(" --- ")
      out.write(l)
      out.write("\n")
    else:
      filter_counts['ignored'] += 1

  return (filtered, filter_counts)

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

#
# Generate regular expressions from schemas.
#

CF_RE = [re.compile(p) for p in CF_PATTERNS]
REQUEST_RE = [re.compile(p) for p in REQUEST_PATTERNS]

CF_SCHEMA_RE = [(re.compile(gen_regex(schema, CF_SCHEMA_DEFS, use_defaults=True)), schema) for \
    schema in CF_SCHEMAS]
REQUEST_SCHEMA_RE = [(re.compile(gen_regex(schema, REQUEST_SCHEMA_DEFS, use_defaults=True)), schema) for \
    schema in REQUEST_SCHEMAS]

IMPL_FORMS = load_stratos_impl_forms()

# Extract transcriptions.
if args.source_type == 'file':
  out = file(args.target_path, 'w')
  filter_file(args.source_path, out)
  out.close()  

else:
  numfile = 0
  filter_counts = defaultdict(int)
  for dirname, subdirlist, filelist in os.walk(args.source_path):
    for f in filelist:
      # Only extract if it's an annotation file.
      print "Extracting file {}".format(numfile)
      numfile += 1
      out = file(os.path.join(args.target_path, f), 'w')
      linemap, countmap = filter_file(os.path.join(dirname, f), out)
      out.close()

      countdict_update(filter_counts, countmap)
      countdict_update(filter_counts, listdict_to_countdict(linemap))
  print filter_counts



