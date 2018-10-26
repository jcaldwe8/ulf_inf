"""
Gene Kim 10-19-2018
Makes sql request files from other files with raw data (e.g. list of ids).
"""

import sys

def usage_message(outfile):
  return "-- use with the command mysql -h ebdb-instance2.cuainl7oscs9.us-east-2.rds.amazonaws.com -P 3306 -u gkim21 -p ebdb --password < " + outfile + " > [out.txt]"

def ids_to_sent_n_ulfs(sids, outfile):
  out = file(outfile, 'w')
  out.write(usage_message(outfile))
  out.write("\n")
  out.write("""select la.sid, s.sentence, la.implicit_ops_added from layer_annotations la
  inner join (select max(id) id, sid
    from layer_annotations
    group by sid)
  unq on unq.id = la.id
  inner join sentences s on s.id = la.sid
  where la.sid in (""")
  out.write(",".join(sids))
  out.write(");")
  out.close()

def ids_to_inf_anns(iids, outfile):
  out = file(outfile, 'w')
  out.write(usage_message(outfile))
  out.write("\n")
  out.write("""SELECT a.sid, a.id, l.id, a.inference, a.category, a.inf_type, a.temporal_reln, a.timestamp
  FROM inf_annotations a, el_annotator_login l 
  WHERE a.auid = l.id
    AND a.id NOT IN ( SELECT annid FROM inf_deleted_annotations )
    AND a.sid in (""")
  out.write(",".join(iids))
  out.write(""")
  ORDER BY a.sid, l.username, a.inf_type;""")
  out.close()


if len(sys.argv) < 5:
  print "ERROR(usage): python make-sql-requests.py [ulf ann id file] [inf ann id file] [generated ULF annotation SQL request file] [generated inference ann SQL request file]"
  sys.exit()

#directory = "../../../oct-2018-inference-subset/revised-sql-direct-ulf-anns/"
#ids_to_sent_n_ulfs(directory + "assigned_ulf_anns.txt", directory + "assigned_inf_anns.txt", "request-sents-n-ulfs.sql")
sids = [l for l in file(sys.argv[1],'r').read().splitlines() if l.strip() != ""]
iids = [l for l in file(sys.argv[2], 'r').read().splitlines() if l.strip() != ""]

ids_to_sent_n_ulfs(sids, sys.argv[3])
ids_to_inf_anns(iids, sys.argv[4])

