"""
Python 2.7 script

Script to extract just the raw dataset from the Project Gutenberg books and
normalize the dataset into a simple format.

This script 
  - removes extraneous info from Project Gutenberg (e.g. upload info)
  - splits the dataset into one sentence per line
  - separates punctuation and contractions into separate tokens

Each sentence (with automatically inferred full stops) is written in one line.

Example call:
  python normalize_dataset.py -spath "data" -stype "dir" -tpath "normalized_dataset"
"""

import argparse
import os
import re
import sys
from collections import defaultdict
from itertools import groupby
from unidecode import unidecode

#
# Constants.
#

CONTEXT_DELIM = "\r\n\r\n"

# TODO: handle utf-8 characters (either by first importing it to ascii, or by handling the characters directly.  Importing to ascii will probably be easier...)

HEADER_DELIM_RE = "^\*\*\* START OF THIS PROJECT GUTENBERG EBOOK .+ \*\*\*$"
FOOTER_DELIM_RE = "^\*\*\* END OF THIS PROJECT GUTENBERG EBOOK .+ \*\*\*$"

#
# Helper functions.
#

# Parses arguments and returns the result of parser.parse_args() from argparse.
def parse_arguments():
  parser = argparse.ArgumentParser(description='Extracts and normalizes sentences from Project Gutenberg text files.')
  
  parser.add_argument('-stype', '--sourcetype', choices=['file', 'dir'], required=True)
  parser.add_argument('-spath', '--sourcepath', type=str, required=True)
  parser.add_argument('-tpath', '--targetpath', type=str, required=True)
  parser.add_argument('-ignore', '--ignorefile', type=str)

  args = parser.parse_args()
  return args

# TODO: turn these splitting functions into libraries since both Project Gutenberg and Discourse Graphbank use these functions.  These will likely be useful elsewhere as well.

# Each pattern must have three groups, where the third group is the sentence boundary.  full_stop_split makes these assumptions when using these patterns.
FULL_STOP_PATTERNS = [
    "(\w)(\.|\?|!)+($| [A-Z'\"`])",
    "(\.|\?|!)+(\"|')+($| [A-Z'\"`])"
    ]

# Split context by inferred full stops.
# NB: this was modified from the version for Discourse Graphbank because the assumptions are different.  In Discourse Graphbank all ends of sentences are followed by a newline, but not in Project Gutenberg files.
def full_stop_split(context):
  # TODO: correct to handle names properly (currently makes a new line in the following names : Mrs. Jones, Jane L. Smith)
  # Full stop if it's a character + period or a period and quotation followed
  # by a newline or a capitalized word.
  # Note the chracter + period is to avoid ellipses as being considered full stops.
  clines = context.splitlines()
  sents = []
  cursent_comps = []
  for l in clines:
    while l.strip() != "":
      l = l.strip()
      matched = False
      for pat in FULL_STOP_PATTERNS:
        res = re.search(pat, l)
        if res:
          matched = True
          sent_boundary = res.span(3)[0]
          cursent_comps.append(l[:sent_boundary])
          sents.append(" ".join(cursent_comps))
          cursent_comps = []
          l = l[sent_boundary:]
      if not matched:
        cursent_comps.append(l)
        l = ""
  if len(cursent_comps) > 0:
    sents.append(" ".join(cursent_comps))
  return sents

# Space-separate punctuation and contractions in given token.
# Call recursively because the phenomena can be stacked.
def expand_token(token, sent_end=False):
  # Identifies the ending substring of input that is punctuation and returns
  # a pair of the prefixing string and the punctuation string.
  # Returns None if there's no ending punctuation.
  # TODO: make sure punctuation is only grouped if the same type and include dashes (-, --) but make sure -- stays together. 
  # TODO: consider splitting hyphonated words.
  def suffix_punct(string):
    for i in range(len(string)):
      letter = string[len(string) - i - 1]
      sent_punct = ["!", ",", "?", ";", ":", "'", "\"", "`", "(", ")", "[", "]", "{", "}"]
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
    return expand_token(token[:-2]) + [token[-2:]]
  elif re.match("^.+(n't|'ve|'ll|'re)$", token):
    # Two letter contractions.
    return expand_token(token[:-3]) + [token[-3:]]
  elif re.match("^\S+(''|\")$", token):
    # Quotes
    return expand_token(token[:-2]) + [token[-2:]]
  elif re.match("^\S+'$", token):
    return expand_token(token[:-1]) + [token[-1:]]
  elif suffix_punct(token):
    # Ending punctuation.
    pre, suf = suffix_punct(token)
    return expand_token(pre) + [suf]
  elif prefix_punct(token):
    # Starting punctuation.
    pre, suf = prefix_punct(token)
    return [pre] + expand_token(suf)
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
  f = file(filename, 'r').read()
  # Convert to nearest ascii counterparts.
  f = unidecode(f.decode("utf-8"))
  contexts = f.split(CONTEXT_DELIM)
  
  # Remove project gutenberg header.
  begin = 0
  for i in xrange(len(contexts)):
    if re.match(HEADER_DELIM_RE, contexts[i]):
      begin = i + 1
      break
  contexts = contexts[begin:]

  # Remove project gutenberg footer.
  end = len(contexts)
  for i in xrange(len(contexts)):
    if re.match(FOOTER_DELIM_RE, contexts[-1 - i]):
      end = -2 - i
      break
  contexts = contexts[:end]

  # Process text.
  for c in contexts:
    for sent in full_stop_split(c):
      # This is done after full_stop_split because the newlines are good signals for full stops.
      flattened = sent.replace("\n", " ").strip()
      expanded = expand_sent(flattened)
      out.write(expanded) 
      out.write("\n")

# Load file containing books to ignore.
# Assume the first entry in each line is the index in the format 
#   /ebooks/[index]
def load_ignorefile(filepath, delim=" "):
  if not filepath:
    return []
  lines = [l for l in file(filepath, 'r').read().splitlines() if l.strip() != '']
  ids = [l.split(" ")[0] for l in lines]
  indices = [int(idstr[8:]) for idstr in ids]
  return indices

FILE_RE_SCHEMA = "^<index>(-\d)?\.txt(\.utf-8)?$"

# Checks if the given filename is in the ignore list.
def in_ignorelist(ignorelist, filename):
  patterns = [FILE_RE_SCHEMA.replace("<index>", str(i)) for i in ignorelist]
  for p in patterns:
    if re.match(p, filename):
      return True
  return False

#
# Main body.
#

args = parse_arguments()

ignorelist = load_ignorefile(args.ignorefile)

# Extract transcriptions.
if args.sourcetype == 'file':
  out = file(args.targetpath, 'w')
  normalize_file(args.sourcepath, out)
  out.close()  

else:
  numfile = 0
  for dirname, subdirlist, filelist in os.walk(args.sourcepath):
    for f in filelist:
      # Only extract if it's not in the ignore list.
      if not in_ignorelist(ignorelist, f):
        print "Extracting file {}".format(numfile)
        numfile += 1
        out = file(os.path.join(args.targetpath, f), 'w')
        normalize_file(os.path.join(dirname, f), out)
        out.close()
      else:
        print "{} in ignore list".format(f)

