"""
Gene Kim 10-28-2018

Merges the tsv files of most recent annotations and most recent annotations 
not done by Gene in the record (so that we can compare differences).
"""

import os
import sys

# NB: copied from combine-and-format-ulf-anns.py in parent dir.
def format_ulf(rawulf):
  newulf = rawulf.replace("\r\\n", "").replace("\\t", "").replace(")(", ") (")
  newnewulf = newulf.replace("  ", " ")
  while newulf != newnewulf:
    newulf = newnewulf
    newnewulf = newulf.replace("  ", " ")
  return newnewulf


if len(sys.argv) < 4:
  sys.exit("Usage: merge_pre_n_post_gene.py [full tsv file] [no gene tsv] [output file]")

# full tsv file format: usid\tsentence\tulf
full_lines = [l.split("\t") for l in file(sys.argv[1]).read().split(os.linesep)[1:] if l.strip() != ""]

# no gene tsv file format: usid\tauid\tcertainty\tsentence\tulf
ng_lines = [l.split("\t") for l in file(sys.argv[2]).read().split(os.linesep)[1:] if l.strip() != ""]

ng_map = { l[0] : l for l in ng_lines }


new_lines = {}

# Merge the lines to : usid\tsentence\tauid\tcertainty\told_ulf\tnew_ulf
# The lines in the full tsv file should be a superset of the no gene file
# -- fail if this is not the case.  The files are probably not right.
for fl in full_lines:
  flsid = fl[0]
  
  if flsid in ng_map:
    ngl = ng_map[flsid]
    assert(ngl[0] == fl[0])
    assert(ngl[3] == fl[1])
    nline = [fl[0], fl[1], ngl[1], ngl[2], ngl[4], fl[2]]
    new_lines[fl[0]] = nline
  else:
    # Gene's the only one that's annotated this.
    nline = [fl[0], fl[1], "n/a", "n/a", fl[2], fl[2]]
    new_lines[fl[0]] = nline

for ngk, ngv in ng_map.iteritems():
  if ngk not in new_lines:
    sys.exit("ERROR: The usids in the most recent annotations is not a superset of the ones without gene's annotations.  The files are probably not corresponding to each other. usid: {}".format(ngk))

# Generates a readable comparison of ULFs of this sid.
def readable_comparison(nl):
  usid = nl[0]
  sent = nl[1]
  auid = nl[2]
  cert = nl[3]
  old_ulf = format_ulf(nl[4])
  new_ulf = format_ulf(nl[5])
  compstr = """SID: {}\tOLD ULF ANNOTATOR ID: {}\tCERTAINTY: {}
  SENTENCE:\t{}
  ORIGINAL ULF:\t{}
  CORRECTD ULF:\t{}""".format(usid, auid, cert, sent, old_ulf, new_ulf)
  return compstr


#out.write("\t".join(["usid", "sentence", "auid", "certainty", "old_ulf", "new_ulf"]))
#out.write("\n")
#out.write("\n".join(["\t".join(l) for k, l in new_lines.iteritems()]))
out = file(sys.argv[3], 'w')
out.write("\n===========================\n".join([readable_comparison(nl) for k, nl in new_lines.iteritems()]))
out.close()


# Generate a file for each annotator.
anntrs = list(set([l[2] for k, l in new_lines.iteritems() if l[2] != "n/a"]))
for a in anntrs:
  out = file(sys.argv[3] + "." + a, 'w')
  out.write("\n============================\n".join([readable_comparison(nl) for k, nl in new_lines.iteritems() if a == nl[2]]))
  out.close()


