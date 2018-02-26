"""
Python 2.7, Gene Kim, 2/26/2018

This script normalizes the UIUC Question Classification dataset.

The sentences are already delimited by sentence and tokenized in the manner
that we want, so all this script does is remove the trainig labels.

Dataset format is [label][space][space separated sentence].
"""

import argparse
import os
import re
import sys
from collections import defaultdict
from itertools import groupby

# Parses arguments and returns the result of parser.parse_args() from argparse.
def parse_arguments():
  parser = argparse.ArgumentParser(description='Removes the labels from the UIUC QC dataset.')
  
  parser.add_argument('-stype', '--sourcetype', choices=['file', 'dir'], required=True)
  parser.add_argument('-spath', '--sourcepath', type=str, required=True)
  parser.add_argument('-tpath', '--targetpath', type=str, required=True)
  
  args = parser.parse_args()
  return args

# Normalizes the file at filepath and write the results to the output stream
# out.
def normalize_file(filepath, out):
  lines = file(filepath, 'r').read().splitlines()
  lines = [" ".join(l.split(" ")[1:]) for l in lines]
  out.write("\n".join(lines))

#
# Main body.
#

args = parse_arguments()

# Extract transcriptions.
if args.sourcetype == 'file':
  out = file(args.target, 'w')
  normalize_file(args.sourcepath, out)
  out.close()  

else:
  numfile = 0
  for dirname, subdirlist, filelist in os.walk(args.sourcepath):
    for f in filelist:
      # Only extract if it's a raw file.
      print "Extracting file {}".format(numfile)
      numfile += 1
      out = file(os.path.join(args.targetpath, f[:-6] + ".raw"), 'w')
      normalize_file(os.path.join(dirname, f), out)
      out.close()


