;;; Gene Kim 11/20/2017
;;; General utility functions.

;; Constructs positive and negative polarity variants of the inferences rule.
;; The positive variant matches the antecedent in only positive contexts.
;; The negative variant matches the consequent in only negative contexts.
;; Returns a pair of new rules in TTT format:
;;  ((/ (ant +) csq)
;;   (/ (csq -) ant))
(defun gen-polarity-rules (r)
  (let ((ant (rule-antecedent r))
        (csq (rule-consequent r)))
    (list (list '/ (append ant '(+)) csq)
          (list '/ (append csq '(-)) ant))))

;; Checks if an element that satisfies pred is a member of lst.
(defun pred-member (pred lst)
  (not (null (remove-if-not pred lst))))

;; Returns the number of times the segment occurs in the formula.
(defun segment-count (s f)
  (labels
    ((helper
       (f1 acc)
       (cond 
         ((null f1) acc)
         ((atom f1) (+ acc (if (equal f1 s) 1 0)))
         (t (helper (cdr f1)
                    (helper (car f1) 
                            (+ acc (if (equal f1 s) 1 0)))))))
     ) ; end of labels defs
    (helper f 0)))



;; One level flatten, i.e. non-recursive.
(defun flatten (lsts) (apply #'append lsts))



;; TODO: move to better place.
;; TODO: complete embedding op list.
;; Sentence embedding operators.
(defparameter *embedding-ops*
  '(tht that n+preds np+preds :l))
(defun emb-op? (x)
  (member x *embedding-ops*))

