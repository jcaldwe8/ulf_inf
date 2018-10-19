"""
Script to generate inference annotation splits.

Everyone but Gene gets 50 annotations.  Gene gets the rest.
"""

ANNTRS = [
  "gene",
  "graeme",
  "ben",
  "georgiy",
  "sophie",
  "muskaan"]


import sys
import random

if len(sys.argv) < 2:
  print "Usage: python gen_inf_ann_splits.py [inf ids]"

ids = [int(e) for e in file(sys.argv[1], 'r').read().splitlines()]
random.shuffle(ids)

anntrids = {}
i = 0
for a in ANNTRS:
  if a != "gene":
    anntrids[a] = ids[i*50:(i+1)*50]
  i += 1

anntrids["gene"] = ids[i*50:]

for a, ids in anntrids.iteritems():
  out = file("inf_{}.ids".format(a), 'w')
  out.write("\n".join([str(e) for e in ids]))
  out.close()

