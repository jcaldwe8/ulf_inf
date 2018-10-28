-- use with the command mysql -h ebdb-instance2.cuainl7oscs9.us-east-2.rds.amazonaws.com -P 3306 -u gkim21 -p ebdb --password.. < request-sid.sql > out.txt
select la.sid, ann_certainty from layer_annotations la
  inner join (select max(id) id, sid
  	from layer_annotations
  	group by sid)
  unq on unq.id = la.id
where la.timestamp > '2018-05-05' and 
auid = 21 and ann_certainty="certain";
