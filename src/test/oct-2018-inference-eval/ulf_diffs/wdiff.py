"""
Gene Kim 10-28-2018

Python word-level diffs?
"""

import difflib
import os
import sys
from pprint import pprint


if len(sys.argv) < 3:
  sys.exit("ERROR: Usage python wdiff.py [file1] [file2]")

str1 = file(sys.argv[1], 'r').read().strip()
str2 = file(sys.argv[2], 'r').read().strip()

# Preprocess strings.
str1new = str1.replace("(", " ( ").replace(")", " ) ").replace("(  (", "((").replace(")  )", "))")
str2new = str2.replace("(", " ( ").replace(")", " ) ").replace("(  (", "((").replace(")  )", "))")


words1 = str1new.split()
words2 = str2new.split()

#diff = difflib.ndiff(words1, words2)
#print "".join(diff)

d = difflib.Differ()
result = list(d.compare(words1, words2))
pprint(result)


