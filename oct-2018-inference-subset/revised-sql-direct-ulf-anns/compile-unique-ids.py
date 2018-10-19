"""
Gene Kim

Reads all *.tsv files in the current folder and merges the sids in the files.  
Each tsv file is assumed to have a header line, followed by each line starting
with the sid of interest.  
"""


import os
import glob

all_tsv_files = glob.glob("./*.tsv")
all_ids = []
iduser_dict = {}
for tf in all_tsv_files:
  user = tf.split("-")[0]
  flines = file(tf, 'r').read().splitlines()
  fids = [int(l.split('\t')[0]) for l in flines[1:] if l.strip() != ""]
  for fid in fids:
    if fid not in iduser_dict:
      iduser_dict[fid] = list()
    iduser_dict[fid].append(user)
  all_ids.extend(fids)

print "length of all ids"
print len(all_ids)
print "length of unique ids"
all_ids = list(set(all_ids))
print len(all_ids)


print "# ids by username"
usercount = {}
userdoublecount = {}
for fid in all_ids:
  users = iduser_dict[fid]
  if len(users) > 1:
    for u in users:
      if u not in userdoublecount:
        userdoublecount[u] = 0
      userdoublecount[u] += 1

  for u in users:
    if u not in usercount:
      usercount[u] = 0
    usercount[u] += 1

for u in usercount:
  print "{}\t{}".format(u, usercount[u])

print "# doubled ids by username"
for u in userdoublecount:
  print "{}\t{}".format(u,userdoublecount[u])


# Get all annotations ids that were assigned and filter out ones that
# don't overlap.  This is in case annotators put in a wrong id, or did
# some miscellaneous annotations.
assigned = file("../original-annotation-lists/all_ex_len.ids", 'r').read().splitlines()
assigned = [int(e) for e in assigned]
print "raw assigned length"
print len(assigned)
print "unique assigned length"
assigned = list(set(assigned))
print len(assigned)

print "annotated filtered by assigned"
assigned_ann = [e for e in all_ids if e in assigned]
print len(assigned_ann)

print "# assigned annotated ids by username"
usercount = {}
userdoublecount = {}
for fid in assigned_ann:
  users = iduser_dict[fid]
  if len(users) > 1:
    for u in users:
      if u not in userdoublecount:
        userdoublecount[u] = 0
      userdoublecount[u] += 1

  for u in users:
    if u not in usercount:
      usercount[u] = 0
    usercount[u] += 1
for u in usercount:
  print "{}\t{}".format(u, usercount[u])

print "# doubled ids by username"
for u in userdoublecount:
  print "{}\t{}".format(u,userdoublecount[u])


#print "not assigned ids by user"
#nassigned = [e for e in all_ids if e not in assigned]
#usernass = {}
#for fid in nassigned:
#  users = iduser_dict[fid]
#  for u in users:
#    if u not in usernass:
#      usernass[u] = list()
#    usernass[u].append(fid)
#
#for u in usernass:
#  print u
#  for sid in usernass[u]:
#    print sid



out = file("all_ulf_anns.txt", 'w')
out.write("\n".join([str(e) for e in all_ids]))
out.close()

out = file("assigned_ulf_anns.txt", 'w')
out.write("\n".join([str(e) for e in assigned_ann]))
out.close()

