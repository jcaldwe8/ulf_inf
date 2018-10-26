"""
Gene Kim 10-25-2018
Combines and formats the inferene annotations into a file for running the experiments.

The input file is of the following format:
  isid\tinfid\tuserid\tinference\tinf-category\tinf_type\ttemporal-reln\ttimestamp
where the first line is the header. 



1. Check inference coverage
2. Map all situational inferences to the recent format.
3. Remove NO INFERENCES FOR CATEGORY entries
"""

import json
import sys
import os

if len(sys.argv) < 3:
  sys.exit("ERROR(usage): python combine-and-format-ulf-anns.py [inf ann file] [formatted inf ann file]")

# File that pulls from SQL to get ulf inference id info.
SID_SENT_ULF_FILE = sys.argv[1]
OUTFILE = sys.argv[2]

rawfile = file(SID_SENT_ULF_FILE, 'r').read()
parsed_infile = [l.split("\t") for l in rawfile.split(os.linesep)[1:] if l.strip() != ""]

# 1. Group by isid and check for coverage in every sentence.
REQUIRED_INF_CATEGORIES = set(["cf", "rq", "q", "impl"])
isid2infs = {}
for pline in parsed_infile:
  isid = pline[0]
  if isid not in isid2infs:
    isid2infs[isid] = []
  isid2infs[isid].append(pline)
cats_missing = []
for isid, plines in isid2infs.iteritems():
  pline_cats = set([pline[4] for pline in plines])
  if len(pline_cats.intersection(REQUIRED_INF_CATEGORIES)) != \
      len(REQUIRED_INF_CATEGORIES):
    cats_missing.append((isid, plines))
    #print pline_cats
    #sys.exit("{} categories not annotated for isid {}.".format(
    #  str(REQUIRED_INF_CATEGORIES.difference(pline_cats)), isid))

if len(cats_missing) > 0:
  sys.exit("cats missing in following isids: ({})".format(
    ",".join([isid for isid, pl in cats_missing])))


# 2. Map situational inferences to the recent format. 

# Old situational annotation contains "situational" in inf_type.
def is_oldsit_line(pline):
  return "situational" in pline[5]

newsit_isid2infs = {}
for isid, plines in isid2infs.iteritems():
  newplines = []
  for pline in plines:
    if is_oldsit_line(pline):
      pline[4] = "sit"
      pline[5] = "situational"
      newplines.append(pline)
    else:
      newplines.append(pline)
  newsit_isid2infs[isid] = newplines

# 3. Remove NO INFERENCES FOR THIS CATEGORY INFERENCES.
def is_noinf_line(pline):
  return pline[5] in ["not-cf", "not-impl", "not-rq", "not-q"]

outlines = []
for isid, plines in newsit_isid2infs.iteritems():
  for pline in plines:
    if not is_noinf_line(pline):
      outlines.append(pline)
    #if is_noinf_line(plines[len(plines) - i - 1]):
    #  del plines[len(plines) - i - 1]

out = file(OUTFILE, 'w')
out.write("isid\tinfid\tuserid\tinf_text\tinf_category\tinf_type\ttemporal_reln\ttimestamp\n")
out.write("\n".join(["\t".join(l) for l in outlines]))
out.close()

