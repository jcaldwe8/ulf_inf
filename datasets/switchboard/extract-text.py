"""
Python 2.7 script

Script to extract just the transcription text from the Switchboard corpus.
Each utterance is formatted to one line with each token separated by a space.
The utterance is indexed by the LDC Penn Treebank id.
Removes:
    1. Dialogue timing
    2. Alignment tag

The script extracts either the LDC Penn Treebank transcriptions, the MS'98 word
transcriptions, or both depending on the command line arguments.

Example call:
  python extract-text.py --inputpath "data" --inputtype "dir" --ldc_target "ldc_transcriptions" --ms_target "ms_transcriptions"
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

SWITCHBOARD_TOKEN_LABELS = ['msid', 'ldcid', 'starttime', 'endtime', 'tag', 'ldc_word', 'ms_word']

BRACKETED_RE = re.compile("\[[\w\-']+\]")
PARTIALLY_BRACKETED_RE = re.compile("[\w\-']+\[[\w\-']+\]")
ALTERNATIVE_BRACKET_RE = re.compile("\[([\w\-']+)/([\w\-']+)\]")
ANNOTATION_NOISE_TOKENS = ["---", "+++"]



#
# Helper functions.
#

# Parses arguments and returns the result of parser.parse_args() from argparse.
def parse_arguments():
  parser = argparse.ArgumentParser(description='Extracts transcriptions from the Switchboard corpus into 1 line per utterance.')
  
  parser.add_argument('-inputtype', '--inputtype', choices=['file', 'dir'], required=True)
  parser.add_argument('-inputpath', '--inputpath', type=str, required=True)
  parser.add_argument('-ldc_target', '--ldc_target', type=str)
  parser.add_argument('-ms_target', '--ms_target', type=str)
  
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



# Extracts transcriptions from the given file to the output streams lout and 
# mout, which represent the LDC transcription output and MS word transcription
# output, respectively.  If the output stream is None, then it is ignored.
def extract_file(filename, lout, mout):
  if not lout and not mout:
    return
  
  flines = file(filename, 'r').read().splitlines()
  tmaps = [line_token_map(l) for l in flines]

  msidmap = defaultdict(list) # Map from MS ids to token maps.

  curid = None
  curtmaps = []
  for tmap in tmaps:
    # If at a boundary write the line out
    if tmap['ldcid'] != curid:
      if curid:
        write_line(curid, curtmaps, lout, mout) 
      curtmaps = [tmap]
      #TODO: see if ms id is more appropriate -- maybe base it off of which file is being generated.
      curid = tmap['ldcid']
    else:
      curtmaps.append(tmap)
  if curid:
    write_line(curid, curtmaps, lout, mout)


def is_bracketed_token(token):
  return re.match(BRACKETED_RE, token)

def is_partially_bracketed_token(token):
  return re.match(PARTIALLY_BRACKETED_RE, token)

def is_annotation_noise(token):
  return token in ANNOTATION_NOISE_TOKENS

def is_filterable(token):
  return is_bracketed_token(token) or \
      is_annotation_noise(token)

def is_alternative_bracket(token):
  return re.match(ALTERNATIVE_BRACKET_RE, token)

def recover_partially_bracketed(token):
  return token.replace("[", "").replace("]", "")

def recover_alternative_bracket(token):
  m = re.match(ALTERNATIVE_BRACKET_RE, token)
  if m and len(m.groups()) > 1:
    return m.group(1)
  else:
    return token


# Cleans the token list to remove discourse annotations or artifacts of the 
# annotations process.
# Removes: ---, +++, [silence], [laughter]
# Cleans: wi[th] -> with
def clean_tokens(tokens):
  filtered = [t for t in tokens if not is_filterable(t)]
  cleaned = []
  for t in filtered:
    if is_partially_bracketed_token(t):
      cleaned.append(recover_partially_bracketed(t))
    elif is_alternative_bracket(t):
      cleaned.append(recover_alternative_bracket(t))
    else:
      cleaned.append(t)
  return cleaned




# TODO: handle discourse words
# TODO: handle incomplete words

args = parse_arguments()




# Extract transcriptions.
if args.inputtype == 'file':
  ldcout = None
  msout = None
  if args.ldc_target:
    ldcout = file(args.ldc_target, 'w')
  if args.ms_target:
    msout = file(args.ms_target, 'w')
  
  extract_file(args.inputpath, ldcout, msout)
  
  if ldcout:
    ldcout.close()
  if msout:
    msout.close()

else:
  numfile = 0
  for dirname, subdirlist, filelist in os.walk(args.inputpath):
    for f in filelist:
      # Only extract if it's an annotation file.
      if f[:2] == "sw" and f[-10:] == "-penn.text":
        print "Extracting file {}".format(numfile)
        numfile += 1
        ldcout = None
        msout = None
        if args.ldc_target:
          ldcout = file(os.path.join(args.ldc_target, f), 'w')
        if args.ms_target:
          msout = file(os.path.join(args.ms_target, f), 'w')

        extract_file(os.path.join(dirname, f), ldcout, msout)

        if ldcout:
          ldcout.close()
        if msout:
          msout.close()








