"""
Python 2.7 script

Script to extract just the raw dataset from the Discourse Graphbank corpus and
normalize the dataset into a simple format.

Each sentence (with automatically inferred full stops) is written in one line.
The original dataset generally contains 1-5 sentences per context. 


Example call:
  python normalize_dataset.py --inputpath "data" --inputtype "dir" --target "normalized_dataset"
"""

import argparse
import os
import re
import sys
from collections import defaultdict
from itertools import groupby

# TODO: make sure full_stop_split handles all the cases correctly (browse over and check if there are any issues).

#
# Constants.
#

CONTEXT_DELIM = "\n\n"
RAW_FILENAME_RE = re.compile("^\d+$")


#
# Helper functions.
#

# Parses arguments and returns the result of parser.parse_args() from argparse.
def parse_arguments():
  parser = argparse.ArgumentParser(description='Extracts transcriptions from the Switchboard corpus into 1 line per utterance.')
  
  parser.add_argument('-inputtype', '--inputtype', choices=['file', 'dir'], required=True)
  parser.add_argument('-inputpath', '--inputpath', type=str, required=True)
  parser.add_argument('-target', '--target', type=str, required=True)
  
  args = parser.parse_args()
  return args

# Split context by inferred full stops.
def full_stop_split(context):
  # Full stop if it's a character + period or a period and quotation followed by a newline.
  # Note the chracter + period is to avoid ellipses as being considered full stops.
  clines = context.splitlines()
  sents = []
  cursent_comps = []
  for l in clines:
    l = l.strip()
    cursent_comps.append(l)
    if (len(l) > 1 and re.match("^\w(\.|\?|!|!\?)$", l[-2:])) or \
        (len(l) > 2 and re.match("^(.|\?|!|!\?)''$", l[-3:])):
      sents.append(" ".join(cursent_comps))
      cursent_comps = []
  if len(cursent_comps) > 0:
    sents.append(" ".join(cursent_comps))
  return sents

# Normalizes given file to one sentence per line and writes them to out.
def normalize_file(filename, out):
  
  f = file(filename, 'r').read()
  contexts = f.split(CONTEXT_DELIM)

  for c in contexts:
    for sent in full_stop_split(c):
      # This is done after full_stop_split because the newlines are good signals for full stops.
      flattened = sent.replace("\n", " ").strip()       
      out.write(flattened) 
      out.write("\n")


#
# Main body.
#

args = parse_arguments()

# Extract transcriptions.
if args.inputtype == 'file':
  out = file(args.target, 'w')
  normalize_file(args.inputpath, out)
  out.close()  

else:
  numfile = 0
  for dirname, subdirlist, filelist in os.walk(args.inputpath):
    for f in filelist:
      # Only extract if it's a raw file.
      if re.match(RAW_FILENAME_RE, f):
        print "Extracting file {}".format(numfile)
        numfile += 1
        out = file(os.path.join(args.target, f), 'w')
        normalize_file(os.path.join(dirname, f), out)
        out.close()







