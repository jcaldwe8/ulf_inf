;;; Gene Kim 10-26-2018
;;;
;;; Inferences for Questions


(defparameter *infer-nomq-from-yn-q-act*
;````````````````````````````````````````
; Did he go? -> whether he did go
; Will he go? -> whether he will go
'(/ ((((!5 tense?) (!6 verbaux?)) ; inverted verb/aux 
      ;(?1 not not.adv-s never.adv-f never.adv-s) ; 
      _!2 ; subj. 
      (?3 not not.adv-s never.adv-f never.adv-s) ; possible negation
      _!4) ; verb phrase
     (? [?]))
    (whether (_!2 ((!5 !6) _!4))))) 

(defun nomq-from-yn-q-act? (tree)
  (let ((res (ttt:apply-rule *infer-nomq-from-yn-q-act* tree :shallow t :max-n 1)))
    (not (equal res tree))))

(defun nomq-from-yn-q-act! (tree)
  (ttt:apply-rule *infer-nomq-from-yn-q-act* tree :shallow t :max-n 1))

(defparameter *infer-you-know-from-yn-q-act*
;`````````````````````````````````````````````````
; Did he go? -> You know whether he did go -> You know whether he went (this last step can be post-processed -- remove unnecessary do.aux-s)
; Will he go? -> You know whether he will go
  '(/ (!1 nomq-from-yn-q-act?)
      (you.pro probably.adv-s 
               ((pres know.v)
                (nomq-from-yn-q-act! !1))))) 

(defparameter *infer-i-not-know-from-yn-q-act*
;`````````````````````````````````````````````````
; Did he go? -> You know whether he did go -> You know whether he went (this last step can be post-processed -- remove unnecessary do.aux-s)
; Will he go? -> You know whether he will go
  '(/ (!1 nomq-from-yn-q-act?)
      (i.pro probably.adv-s 
               ((pres do.aux-s) not 
                (know.v (nomq-from-yn-q-act! !1)))))) 

(defun wh? (x)
  (member x '(what.pro when.pq where.pq who.pro whom.pro how.adv-s why.adv-s)))

(defparameter *infer-nomq-from-wh-q-act*
;`````````````````````````````````````````````````
; Where did you go? -> where you did go
; Who did you meet? -> who you did meet
; TODO: which dog did you pet? -> which dog you did pet; how long did you stay; what day is it
  '(/ ((sub (!7 wh?)
       (((!5 tense?) (!6 verbaux?)) ; inverted verb/aux 
        ;(?1 not not.adv-s never.adv-f never.adv-s) ; 
        _!2 ; subj. 
        (?3 not not.adv-s never.adv-f never.adv-s) ; possible negation
        _!4)) ; verb phrase
       (? [?]))

        (ans-to (sub !7 (_!2 ((!5 !6) _!4)))))) 
(defun nomq-from-wh-q-act? (tree)
  (let ((res (ttt:apply-rule *infer-nomq-from-wh-q-act* tree :shallow 1 :max-n 1)))
    (not (equal res tree))))
(defun nomq-from-wh-q-act! (tree)
  (ttt:apply-rule *infer-nomq-from-wh-q-act* tree :shallow t :max-n 1))


(defparameter *infer-you-know-from-wh-q-act*
;`````````````````````````````````````````````````
; Where did you go? -> You know where you did go -> You know where you went
; Who did you meet? -> You know who you did meet -> You know who you met
  '(/ (!9 nomq-from-wh-q-act?)
      (you.pro probably.adv-s 
               ((pres know.v)
               (nomq-from-wh-q-act! !9)))))

(defparameter *infer-i-not-know-from-wh-q-act*
  '(/ (!9 nomq-from-wh-q-act?)
      (i.pro probably.adv-s  ((pres do.aux-s) not
               (know.v
                (nomq-from-wh-q-act! !9)))))) 

(defun infer-you-know-from-yn-q-act (ulf)
  (all-ttt-rule-inf-result *infer-you-know-from-yn-q-act* ulf))
(defun infer-i-not-know-from-yn-q-act (ulf)
  (all-ttt-rule-inf-result *infer-i-not-know-from-yn-q-act* ulf))
(defun infer-you-know-from-wh-q-act (ulf)
  (all-ttt-rule-inf-result *infer-you-know-from-wh-q-act* ulf))
(defun infer-i-not-know-from-wh-q-act (ulf)
  (all-ttt-rule-inf-result *infer-i-not-know-from-wh-q-act* ulf))

; [raw variants]
; The *-raw variants of the rules return lists of formulas instead of
; 'inf-result instances.
; 
(defun infer-you-know-from-yn-q-act-raw (ulf)
  (mapcar #'result-formula
          (infer-you-know-from-yn-q-act ulf)))
(defun infer-i-not-know-from-yn-q-act-raw (ulf)
  (mapcar #'result-formula
          (infer-i-not-know-from-yn-q-act ulf)))
(defun infer-you-know-from-wh-q-act-raw (ulf)
  (mapcar #'result-formula
          (infer-you-know-from-wh-q-act ulf)))
(defun infer-i-not-know-from-wh-q-act-raw (ulf)
  (mapcar #'result-formula
          (infer-i-not-know-from-wh-q-act ulf)))



(defun premacro-q-inferences (ulf)
  (cdar (results-from-applying-rules
          (list #'infer-you-know-from-yn-q-act
                #'infer-i-not-know-from-yn-q-act
                #'infer-you-know-from-wh-q-act
                #'infer-i-not-know-from-wh-q-act))
        (list ulf) t))
        

