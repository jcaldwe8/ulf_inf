"""
Takes a file with ULF sids and a dataset json file and outputs a file with 
corresponding inference sids. Order is not preserved.
"""


import sys
import json

if len(sys.argv) < 4:
  sys.exit("Usage: python ulf_sid_to_inf_sid.py [dataset json file] [ulf sid file] [output file]")


ulfsids = set([int(e) for e in file(sys.argv[2], 'r').read().splitlines()])
u2isid = {}
out = file(sys.argv[3], 'w')
dataset = json.loads(file(sys.argv[1], 'r').read())

i = 1
for e in dataset:
  if i % 10000 == 0:
    print "i: {}".format(i)
  i += 1
  if e["ulf_sentences_id"] in ulfsids:
    u2isid[e["ulf_sentences_id"]] = e["inf_sentences_id"]

out.write("\n".join(["{}\t{}".format(u, i) for u, i in u2isid.iteritems()]))
#out.write("\n".join(["{}".format(i) for u, i in u2isid.iteritems()]))
out.close()

