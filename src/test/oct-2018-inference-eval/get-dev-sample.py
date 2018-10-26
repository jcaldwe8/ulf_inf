"""
Gene Kim 10-25-2018
Get's the sample for the development set of the experiment.
"""

import random
import sys

if len(sys.argv) < 4:
  sys.exit("Usage: python script.py [input usids] [dev usid file] [test usid file]")

DEVSIZE = 40

usids = [l.split("\t")[0] for l in file(sys.argv[1], 'r').read().splitlines()[1:] if l.strip() != ""]

dev_usids = random.sample(usids, DEVSIZE)
test_usids = [usid for usid in usids if usid not in dev_usids]

devout = file(sys.argv[2], 'w')
devout.write("\n".join(dev_usids))
devout.close()

testout = file(sys.argv[3], 'w')
testout.write("\n".join(test_usids))
testout.close()

