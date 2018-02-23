"""
Python 2.7

Script to filter out smaller lists from the Uppen morphological data.

Example call (to get past tense and past participle entries):
  python uppen_morph.py --source "morph-1.5/data/morph_english.flat" --target "filtered_lists/english_past_and_ppart.flat" --poses "V" --features "PAST" "PPART"
"""

import argparse
import os
from collections import namedtuple

LemmaInfo = namedtuple('LemmaInfo', ['lemma', 'pos', 'features'])
MorphEntry = namedtuple('MorphEntry', ['token', 'lemmas'])

# Rudimentary function to determine if a line is commented:
# (i.e. first characteris a semicolon)
def is_commented(l):
  return len(l) > 0 and l[0] == ';'

# Rudimentary function to determine if the line is a morphology data entry.
def is_morph_entry(l):
  return len([t.strip() for t in l.split('\t') if t.strip() != '']) > 2

# Define and parse command line arguments.
def parse_arguments():
  parser = argparse.ArgumentParser(description="Filters the Uppen Morphological data file into simpler smaller subsets.")
  parser.add_argument('-source', '--source', type=str, required=True)
  parser.add_argument('-target', '--target', type=str, required=True)
  parser.add_argument('-poses', '--poses', type=str, nargs='*')
  parser.add_argument('-features', '--features', type=str, nargs='*') 
  args = parser.parse_args()
  return args

# Reads a morph data line and returns a MorphEntry tuple.
# Each line consists of 
#   [word token]\t[lemma info 1]#...#[feature list n]
# where [lemma info m] consists of
#   [lemma]\t[space separated features]
# where the space separated features start with POS.
# NB: surely there's more structure on the features, but I'm not too interested
#     in that right now.
def read_morph_line(l):
  token, info = [s.strip() for s in l.split('\t', 1)]
  lemmatexts = [lt for lt in (e.strip() for e in info.split('#')) if lt != '']
  # For each lemma, separate by space, strip, and organize into namedtuple.
  lemmasplit = [(lemma, info) for lemma, info in ([e1.strip() for e1 in e.split('\t')] for e in lemmatexts)]
  lemmas = [LemmaInfo(lemma=lemma, pos=ts[0], features=ts[1:]) for lemma, ts in \
      ((lem, [e.strip() for e in info.split(' ')]) for lem, info in lemmasplit)]
  return MorphEntry(token=token, lemmas=lemmas)

# Reads a morph file and returns a list of MorphEntry tuples.
def load_morph_file(f="morph-1.5/data/morph_english.flat"):
  fullpath = os.path.join(os.path.dirname(__file__), f)
  print "Loading morph file..."
  lines = file(fullpath, 'r').read().splitlines()
  entries = [read_morph_line(l) for l in lines if is_morph_entry(l)]
  print "Morph file loaded."
  return entries

# Checks if the LemmaInfo tuple has one of the POS tags in poses and that
# at least one of its features is in the features list.
# If features is an empty list, it is ignored -- only the POS tag must match
# one in poses.
def contains_feature(linfo, poses, features):
  if linfo.pos in poses:
    if len(linfo.features) == 0:
      return True
    for f in linfo.features:
      if f in features:
        return True
  return False

# Filters the morphological data to so that only entries containing something
# in the feature list is included.  If features is an empty list all features
# are accepted (i.e. filtering is only based on POS tags).
# TODO: generalize so that we can specify more complicated feature structures
# (e.g. both features, included).
def filter_morph_data(entries, poses, features):
  # Helper function (restructures the MorphEntry tuple according to filter).
  def filter_morph_entry(entry):
    lis = [li for li in entry.lemmas if contains_feature(li, poses, features)]
    if len(lis) > 0:
      return MorphEntry(token=entry.token, lemmas=lis)
    else:
      return None
  # Apply filter to all.
  return [f for f in (filter_morph_entry(e) for e in entries) if f] 

def lemmainfo_str(lemmainfo):
  return lemmainfo.lemma + "\t" + \
      " ".join([lemmainfo.pos] + lemmainfo.features)

def morphentry_str(entry):
  return entry.token + "\t" + \
      "#".join([lemmainfo_str(li) for li in entry.lemmas])

def write_entries(filepath, entries):
  out = file(filepath, 'w')
  for e in entries:
    out.write(morphentry_str(e))
    out.write("\n")
  out.close()

def main():
  args = parse_arguments()
  entries = load_morph_file(args.source)
  filtered = filter_morph_data(entries, args.poses, args.features)
  write_entries(args.target, filtered)

if __name__ == '__main__':
  main()

