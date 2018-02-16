
import sys
import codecs

# Checks the coverage of a verb list in the e_lemma.txt resource.

# Read e_lemma.txt
#efile = codecs.open('e_lemma/e_lemma.txt', encoding='utf-8')
elines = open('e_lemma/e_lemma.txt', 'r', errors='ignore').read().splitlines()
#for el in efile:
#  elines.append(repr(el))
elines = [str(l) for l in elines]
elines = [l.replace('\x00','') for l in elines if len(l.strip()) != 1]

el1 = elines[0]
el2 = elines[30]
print(el1)
print(el2)

print(el1[0])
print(el1[0] == ';')
elines = [l for l in elines if ';' not in l[:1]]

ewords = [l.split('->')[0].strip() for l in elines if l[0] != ';']

# Read verb list.
verbs = open(sys.argv[1], 'r').read().splitlines()

covered = [v for v in verbs if v in ewords]

print("Covered {} out of {}".format(len(covered), len(verbs)))
print("Uncovered verbs")
print('\n'.join([v for v in verbs if v not in ewords]))
