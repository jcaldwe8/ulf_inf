"""
Python 2.7 script

Script to extract just the raw dataset from the Monroe Spoken Corpus and
normalize the dataset into a simple format.


Example call:
  python normalize_dataset.py -spath "data" -stype "dir" -tpath "normalized_dataset"
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

CONTEXT_DELIM_RE = "utt\d(?:\d| )(?:\d| )\: (?:s\:|u\:|  )  "
BRACKETED_RE = re.compile("\[[\w\-']+\]")
ANGLE_BRACKETED_RE = re.compile("\<[\w\-']+\>")
RAW_FILENAME_RE = re.compile("^s\d+\.transcript$")


#
# Helper functions.
#

# Parses arguments and returns the result of parser.parse_args() from argparse.
def parse_arguments():
  parser = argparse.ArgumentParser(description='Extracts transcriptions from the Monroe Spoken Dialogue corpus into 1 line per utterance.')
  
  parser.add_argument('-stype', '--sourcetype', choices=['file', 'dir'], required=True)
  parser.add_argument('-spath', '--sourcepath', type=str, required=True)
  parser.add_argument('-tpath', '--targetpath', type=str, required=True)
  
  args = parser.parse_args()
  return args

# Normalizes given file to one sentence per line and writes them to out.
def normalize_file(filename, out):
  f = file(filename, 'r').read()
  utterances = re.compile(CONTEXT_DELIM_RE).split(f)[1:] # first segment is metadata.
  #print "\n".join(utterances)
  #sys.exit() 
  for u in utterances:
    line = u.replace("\n", " ").strip()
    line = line.replace("+ ", "").replace(" +", "")
    line = re.sub(BRACKETED_RE, "", line)
    line = re.sub(ANGLE_BRACKETED_RE, "", line)  
    line = " ".join([t for t in line.split(" ") if t.strip() != ""])
    out.write(line) 
    out.write("\n")


#
# Main body.
#

args = parse_arguments()

# Extract transcriptions.
if args.sourcetype == 'file':
  out = file(args.targetpath, 'w')
  normalize_file(args.sourcepath, out)
  out.close()  

else:
  numfile = 0
  for dirname, subdirlist, filelist in os.walk(args.sourcepath):
    for f in filelist:
      # Only extract if it's a raw file.
      if re.match(RAW_FILENAME_RE, f):
        print "Extracting file {}".format(numfile)
        numfile += 1
        out = file(os.path.join(args.targetpath, f), 'w')
        normalize_file(os.path.join(dirname, f), out)
        out.close()







