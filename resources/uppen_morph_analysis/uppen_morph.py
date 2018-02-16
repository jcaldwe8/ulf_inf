"""
Python 2.7

Script to filter out smaller lists from the Uppen morphological data.

Example call (to get past tense and past participle entries):
  python uppen_morph.py --source "morph-1.5/data/morph_english.flat" --target "filtered_lists/english_past_and_ppart.flat" --poses "V" --features "PAST" "PPART"
"""

import argparse
import os
from collections import namedtuple

Features = namedtuple('Features', ['pos', 'features'])
MorphEntry = namedtuple('MorphEntry', ['token', 'lemma', 'featurelists'])

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
#   [word token]\t[lemma]\t[feature list 1]\t...\t[feature list n]
# where each feature list is space separated with the first feature being the
# POS.
def read_morph_line(l):
  segments = [s for s in (s.strip() for s in l.split('\t')) if s != '']
  assert len(segments) > 2, "{}".format(l)
  flists = [Features(pos=ts[0], features=ts[1:]) for ts in \
      (s.split(' ') for s in segments[2:])]
  return MorphEntry(token=segments[0], lemma=segments[1], featurelists=flists)

# Reads a morph file and returns a list of MorphEntry tuples.
def load_morph_file(f="morph-1.5/data/morph_english.flat"):
  fullpath = os.path.join(os.path.dirname(__file__), f)
  print "Loading morph file..."
  lines = file(fullpath, 'r').read().splitlines()
  entries = [read_morph_line(l) for l in lines if is_morph_entry(l)]
  print "Morph file loaded."
  return entries

# Checks if the Feature tuple has one of the POS tags in poses and that
# at least one of its features is in the features list.
def contains_feature(ftuple, poses, features):
  if ftuple.pos in poses:
    for f in ftuple.features:
      if f in features:
        return True
  return False

# Filters the morphological data to so that only entries containing something
# in the feature list is included.
# TODO: generalize so that we can specify more complicated feature structures
# (e.g. both features, included).
def filter_morph_data(entries, poses, features):
  # Helper function (restructures the MorphEntry tuple according to filter).
  def filter_morph_entry(entry):
    ff = [f for f in entry.featurelists if contains_feature(f, poses, features)]
    if len(ff) > 0:
      return MorphEntry(token=entry.token, lemma=entry.lemma, featurelists=ff)
    else:
      return None
  # Apply filter to all.
  return [f for f in (filter_morph_entry(e) for e in entries) if f] 

def features_str(features):
  return " ".join([features.pos] + features.features)

def morphentry_str(entry):
  return "\t".join([entry.token, entry.lemma] + \
      [features_str(fl) for fl in entry.featurelists])

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

