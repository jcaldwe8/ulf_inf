"""
Gene Kim 10-19-2018
Combines and formats the ULF annotations into a file for running the experiments.
"""

import json
import sys
import os

if len(sys.argv) < 4:
  sys.exit("ERROR(usage): python combine-and-format-ulf-anns.py [sid to ulf file] [dataset json file] [generated sid iid file]")

# Removes unnecessary newlines and tabs.
# We leave in extra spaces since lisp can do a better job of filtering those.
# TODO: remember to do the names preprocessing before inputting into Lisp.
def format_ulf(rawulf):
  return rawulf.replace("\r\\n", "").replace("\\t", "")

def format_sent(sent):
  return sent.replace("\\n", "")

# File that pulls from SQL to get ulf inference id info.
# TODO: the SQL database should be updated to include a correspondence between the inference sentence ids and the ulf sentence ids, rather than doing it in python...

#SID_SENT_ULF_FILE = "sid-sent-ulf-out.tsv"
#DATASET_JSON_FILE = "../../../oct-2018-inference-subset/source_dataset.json"
#OUTFILE = "usid-isid-sent-ulf-out.tsv"
SID_SENT_ULF_FILE = sys.argv[1]
DATASET_JSON_FILE = sys.argv[2]
OUTFILE = sys.argv[3]

rawfile = file(SID_SENT_ULF_FILE, 'r').read()
parsed_ssuf = [l.split("\t") for l in rawfile.split(os.linesep)[1:] if l.strip() != ""]
#print "\n".join([l[0] for l in parsed_ssuf])
#print len(parsed_ssuf)
ulfsids = [int(l[0]) for l in parsed_ssuf]
u2isid = {}

dataset = json.loads(file(DATASET_JSON_FILE, 'r').read())

i = 0
for e in dataset:
  i += 1
  if e["ulf_sentences_id"] in ulfsids:
    u2isid[e["ulf_sentences_id"]] = e["inf_sentences_id"]

sisuf = [[l[0], str(u2isid[int(l[0])]), format_sent(l[1]), format_ulf(l[2])] for l in parsed_ssuf]

out = file(OUTFILE, 'w')
out.write("usid\tisid\tsentence\tulf\n")
out.write("\n".join(["\t".join(l) for l in sisuf]))
out.close()

