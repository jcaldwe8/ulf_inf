"""
Python 2.7 script

Script to extract just the raw dataset from the Tatoeba corpus and
normalize the dataset into a simple format.

Based on the version used for Discouse Graphbank.  This is a simplification
since Tatoeba doesn't require sentence delimiting.

Example call:
  python normalize_dataset.py -spath "data" -stype "dir" -tpath "normalized_dataset"
"""

import argparse
import os
import re
import sys
from collections import defaultdict
from itertools import groupby


#
# Constants.
#

CONTEXT_DELIM = "\n"
RAW_FILENAME_RE = re.compile("^\d+$")


#
# Helper functions.
#

# Parses arguments and returns the result of parser.parse_args() from argparse.
def parse_arguments():
  parser = argparse.ArgumentParser(description='Extracts transcriptions from the Switchboard corpus into 1 line per utterance.')
  
  parser.add_argument('-stype', '--sourcetype', choices=['file', 'dir'], required=True)
  parser.add_argument('-spath', '--sourcepath', type=str, required=True)
  parser.add_argument('-tpath', '--targetpath', type=str, required=True)
  
  args = parser.parse_args()
  return args

# Space-separate punctuation and contractions in given token.
# Call recursively because the phenomena can be stacked.
def expand_token(token, sent_end=False):
  # Identifies the ending substring of input that is punctuation and returns
  # a pair of the prefixing string and the punctuation string.
  # Returns None if there's no ending punctuation.
  def suffix_punct(string):
    for i in range(len(string)):
      letter = string[len(string) - i - 1]
      sent_punct = ["!", ",", "?", ";", "'", "\"", "`", "(", ")", "[", "]", "{", "}"]
      if sent_end:
        sent_punct.append(".")
      if letter not in sent_punct:
        if i == 0:
          return None
        else:
          return (string[:len(string) - i], string[len(string) - i:])
    return None
  # Reverse of suffix_punct
  def prefix_punct(string):
    if suffix_punct(string[::-1]):
      pre, suf = suffix_punct(string[::-1])
      return (suf[::-1], pre[::-1])
    else:
      return None

  if re.match("^.+('s|'d|'m)$", token):
    # One letter contractions.
    return expand_token(token[:-2], sent_end) + [token[-2:]]
  elif re.match("^.+(n't|'ve|'ll|'re)$", token):
    # Two letter contractions.
    return expand_token(token[:-3], sent_end) + [token[-3:]]
  elif re.match("^\S+('')$", token):
    # Expanded Double Quotes
    return expand_token(token[:-2], True) + [token[-2:]]
  elif re.match("^\S+(\")$", token):
    # Collapsed Double Quotes
    return expand_token(token[:-1], True) + [token[-1:]]
  elif re.match("^\S+'$", token):
    # Single Quotes
    return expand_token(token[:-1], True) + [token[-1:]]
  elif suffix_punct(token):
    # Ending punctuation.
    pre, suf = suffix_punct(token)
    return expand_token(pre, sent_end) + [suf]
  elif prefix_punct(token):
    # Starting punctuation.
    pre, suf = prefix_punct(token)
    return [pre] + expand_token(suf, sent_end)
  else:
    return [token]

# Space-separate punctuation and contractions.
def expand_sent(sent):
  tokens = sent.split(" ")
  newtokens = []
  for t in tokens[:-1]:
    newtokens.extend(expand_token(t))
  newtokens.extend(expand_token(tokens[-1], sent_end=True))
  return " ".join(newtokens)

# Normalizes given file to one sentence per line and writes them to out.
def normalize_file(filename, out):
  
  lines = file(filename, 'r').read().splitlines()
  i = 0
  for l in lines:
    if i % 1000 == 0:
      print "Completed {} lines".format(i)
    i += 1
    if l.strip() != "":
      expanded = expand_sent(l)
      out.write(expanded)
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

