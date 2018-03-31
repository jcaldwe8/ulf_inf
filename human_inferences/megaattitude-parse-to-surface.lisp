;; Script to convert megaattitude parses to surface form.
;;  
;;  (VB* (VP <verb>) (TENSE <tense>)) -> (<tense> <verb>) -(lookup)-> surface
;;  (VB* (VP <verb>) (ASP <asp>)) -> (<asp> <verb>) -(lookup)-> surface
;;
;;
;;
;; To ULF:
;;  someone -> x
;;  something -> y
;;  (VB* (VB be) (TENSE <tense>)) (VBN (VB <verb>) (ASP PASTPART)) -> (<tense> (pasv <verb>))
;;  (VP (TO to) (VP ...)) -> (to [...])
;; TODO: similar to ULF
;;
;;
;; So if we can have annotators mark the type of inference being done (at a
;; structural level via bleached arguments), then we can write the inference
;; engine based off of mainly the bleached patterns and run it on the dataset.
;; We can quantify the reduction in referred sentences (number of bleached
;; sentences) as well as sentence complexity.  We'll still want to refer to a
;; sample of the sentences to handle some of the syntactic complexities that
;; don't get captured in bleached rules (e.g. deep embeddings, quotes, etc.).
;;
