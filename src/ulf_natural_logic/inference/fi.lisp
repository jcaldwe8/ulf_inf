;;; Gene Kim, 11/20/2017
;;; Forward Inference.



;; Filters the list of rules to only those that can be applied for formula.
;; Input
;;  f: Formula
;;  rs: Inference Rules
;; Return
;;  pos-neg: pair of lists where the first list contains rules where the 
;;           antecedent matches in positive polarity, and the second list
;;           contains rules where the consequent matches in negative polarity.
(defun can-apply (f rs)
  (let* ((rs-ants (mapcar #'(lambda (x) (list x (rule-antecedent x)))
                          r))
         (rs-split (mapcar #'(lambda (x) 
                               (append x (list (rule-consequent (first x)))))
                           rs-ants)))
    (labels (
             ;; New pos-neg.
             ;; Checks the formula segment against rs-split and adds
             ;; appropriate rules to pos-neg.
             (update-posneg
               (fseg posneg)
               (let ((amatch-rs (remove-if-not 
                                  #'(lambda (x) (match-fseg (second x) fseg))
                                  rs-split))
                     (cmatch-rs (remove-if-not
                                  #'(lambda (x) (match-fseg (third x) fseg))
                                  rs-split)))
                 (list
                   (append amatch-rs (first posneg))
                   (append cmatch-rs (second posneg)))))

             ;; Helper function that does all the heavy lifting.
             (helper
               (f' acc)
               (cond 
                 ;; Base case.
                 ((null f') acc)
                 ;; Recurse.
                 (t 
                   (let* ((e (car f'))
                          (acc' (update-posneg e acc))
                          (acc'' (if (atom e) acc'
                                   (helper e acc'))))
                     (helper (cdr f') acc'')))))
             ) ; end of labels defs
      ;; Start helper function.
      (helper f nil))))
  
























