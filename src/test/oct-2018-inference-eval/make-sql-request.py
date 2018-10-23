"""
Gene Kim 10-19-2018
Makes sql request files from other files with raw data (e.g. list of ids).
"""

import sys

def ids_to_sent_n_ulfs(sidfile, iidfile, outfile):
  ids = [l for l in file(sidfile,'r').read().splitlines() if l.strip() != ""]
 
  # TODO: include inf id....
  out = file(outfile, 'w')
  out.write("""-- use with the command mysql -h ebdb-instance2.cuainl7oscs9.us-east-2.rds.amazonaws.com -P 3306 -u gkim21 -p ebdb --password < """)
  out.write(outfile)
  out.write(""" > [out.txt]
  select la.sid, s.sentence, la.implicit_ops_added from layer_annotations la
  inner join (select max(id) id, sid
  	from layer_annotations
  	group by sid)
  unq on unq.id = la.id
  inner join sentences s on s.id = la.sid
where la.sid in (""")
  out.write(",".join(ids))
  out.write(");""")
  out.close()


if len(sys.argv) < 4:
  print "ERROR(usage): python make-sql-request.py [ulf ann id file] [inf ann id file] [generated SQL request file]"
  sys.exit()

#directory = "../../../oct-2018-inference-subset/revised-sql-direct-ulf-anns/"
#ids_to_sent_n_ulfs(directory + "assigned_ulf_anns.txt", directory + "assigned_inf_anns.txt", "request-sents-n-ulfs.sql")
ids_to_sent_n_ulfs(sys.argv[1], sys.argv[2], sys.argv[3])

