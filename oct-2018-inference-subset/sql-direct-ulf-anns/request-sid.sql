select la.sid, ann_certainty from layer_annotations la
  inner join (select id, sid, max(timestamp)
  	from layer_annotations
  	group by sid)
  unq on unq.id = la.id
where la.timestamp > '2018-05-05' and auid = 29 and ann_certainty="certain";
