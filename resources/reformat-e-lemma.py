
import sys
import codecs

# Removes extraneous encoding noise when reading into python and writes it into
# an equivalent file with easier encoding to work with.

# Read e_lemma.txt
elines = open('e_lemma/e_lemma.txt', 'r', errors='ignore').read().splitlines()
elines = [str(l) for l in elines]
elines = [l.replace('\x00','') for l in elines if len(l.strip()) != 1]

out = open(sys.argv[1], 'w')
out.write("\n".join(elines))
out.close()

