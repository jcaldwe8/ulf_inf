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
UPPEN_MORPH_PATH = "resources/uppen_morph_analysis"
CUSTOMIZED_RESOURCE_PATH = "resources/customized_resources"

# Load local functions.
sys.path.append(UPPEN_MORPH_PATH)
from uppen_morph import load_morph_file 

sys.path.append(CUSTOMIZED_RESOURCE_PATH)
from customized_resources import load_stratos_impl_forms

#
# Global vars.
#

# TODO: handle punctuation preceding sentence -- e.g. quoted words.  Probably want to just preprocess the datasets to separate the quotes and punctuation from words so we don't have to deal with this... So the tokens are most normalized.

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
    # TODO: generalize the capitalization
    ".* (would|will|can|could) you .*",
    ".* (I|We|we|You|you) need .*",
    ".* may I .*",
    "Could I .*",
    # Negative requests (aka "false requests")
    ".*(won't|wouldn't|can't|couldn't) .*\?",
    ".*(would|could|wouldn't|couldn't|won't|can't) you .*",
    ".*(must|Must) you .*"
    ]

CF_RE = [re.compile(p) for p in CF_PATTERNS]
REQUEST_RE = [re.compile(p) for p in REQUEST_PATTERNS]

IMPL_FORMS = load_stratos_impl_forms()

#
# Helper functions.
#

# Parses arguments and returns the result of parser.parse_args() from argparse.
def parse_arguments():
  parser = argparse.ArgumentParser(description='Extracts sentences that may have phenomena of interest from a dataset.')
  
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
  for p in CF_RE:
    if re.match(p, l):
      return p.pattern
  return None

def is_request(l):
  for p in REQUEST_RE:
    if re.match(p, l):
      return p.pattern
  return None

def is_question(l):
  # TODO
  return False

def is_communicative(l):
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
      is_communicative(l) or is_implicative(l)





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
      if is_communicative(l):
        filtered['communicative'].append(l)
        out.write(str(is_communicative(l)) + "[communicative]")
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



