;;; Gene Kim 11/20/2017
;;; Functions for running inferences and tying together different components.

;; TODO: implement BFS inference.


;; Return rule pair that matches.  Nil if none.
(defun test-rules (f rulepairs)
  (cond
    ((null rulepairs) nil)
    (t (let ((rp (car rulepairs)))
         (if (ttt:match-expr (first rp) f)
           rp (test-rules f (cdr rulepairs)))))))



;; Returns (inf-rule used, new formula).
;; Assumes a polarity marked formula as handled in polarity-util.lisp.
(defun impl-inf-step (f)
  (if *debug* (format t "impl-inf-step; f ~s~%~%" f))
  (cond
    ;; Bad inputs.
    ((atom f)
     ;; TODO: raise actual error, so it can be processed appropriately...
     (format t "ERROR: Came across atom in impl-inf-step: ~s~%~%" f))
    ((not (equal 2 (length f)))
     (format t "ERROR: Polarity marked logical segment as wrong length in impl-inf-step: ~s~%~%" f))
    ;; Good input, perform inference step.
    (t (let* ((polar (extract-polarity f))
              (logic (extract-logic f))
              ;; Retrieve possible inference rules.
              ;; TODO: add inference rule selection.
              ;; rules: list of pairs (antecedent -- sensitive to polarity,
              ;;                       full rule)
              (rules (cond
                       ((equal '+ polar) (flatten *pos-rules*))
                       ((equal '- polar) (flatten *neg-rules*))
                       (t nil)))
              ;; Apply inferences.

              ;; infs: list of successful inference pairs 
              ;;       (rule used, result of inference)
              (matched-rules (remove-if-not
                               #'(lambda (x) (ttt:match-expr (first x) logic))
                               rules))
              (infs (mapcar 
                      #'(lambda (x) (list x (propagate-polarity
                                              (ttt:apply-rule (second x) logic)
                                              :polarity polar)))
                      matched-rules)))
         ;(format t "logic ~s~%matched-rules ~s~%~%" logic matched-rules)
         (cond
           ;; Leaf node, no recursing.
           ((atom logic)
            ;(format t "atom-infs ~s~%~%" infs)
            ;TODO: choose inference instead of taking the first.
            (if infs (first infs) (list nil f)))
    
           ;; Internal node, recurse if no inference.
           (t (if infs  
                ;; If we got an inference, then return it.
                (first infs)
                ;; If we did't get an inference, recurse.
                (let* ((leftres (impl-inf-step (first logic)))
                       ;; Compute right recursion if left gives no result.
                       (rightres (if (first leftres) nil
                                   (impl-inf-step (second logic)))))
                  (if (first leftres)
                    ;; If recursing left gets a result, combine inferred 
                    ;; left branch with right branch.
                    (list (first leftres) 
                          (propagate-polarity 
                            (list (second leftres) (second logic))
                            :polarity polar))
                    ;; Combine left branch with inferred right branch.
                    (list (first rightres) 
                          (propagate-polarity
                            (list (first logic) (second rightres))
                            :polarity polar)))))))))))




;; Applies an equality rule to a logical segment.
(defun apply-eq-rule (r l)
  (let ((eq-ttt (list '/ (second r) (third r)))) ; replace = with / for TTT.
    (ttt:apply-rule eq-ttt l)))

;; Equality inference doesn't care about polarity.  Always possible.
(defun equal-inf-step (f)
  ;(format t "equal-inf-step; f ~s~%~%" f)
  (cond
    ;; Bad inputs.
    ((atom f) 
     (format t "ERROR: Came across atom in equal-inf-step: ~s~%~%" f))
    ((not (equal 2 (length f)))
     (format t "ERROR: Polarity marked logical segment as wrong lengthin equal-inf-step: ~s~%~%" f))

    (t (let* ((logic (extract-logic f))
              (polar (extract-polarity f))
              (rules (flatten *eq-rules*))         
              (matched-rules (remove-if-not
                            #'(lambda (x) (ttt:match-expr (first x) logic))
                            rules))
              ;; Apply inferences.
              (infs (mapcar 
                          #'(lambda (x) (list x 
                                              (propagate-polarity 
                                                (apply-eq-rule (second x) logic)
                                                :polarity polar)))
                          matched-rules)))
         ;(format t "equal-inf-step; matched-rules ~s~%infs ~s~%~%" matched-rules infs)
         ;; NB: below here is almost identical to impl-inf-step (gross...)
         ;;     should probably refactor...
         (cond
           ;; Leaf node, no recursing.
           ((atom logic)
            ;(format t "Logical atom; infs ~s~%f ~s~%~%" infs f)
            (if infs (first infs) (list nil f)))
    
           ;; Internal node, recurse if no inference.
           (t (if infs  
                ;; If we got an inference, then return it.
                (first infs) 
                ;; If we did't get an inference, recurse.
                (let* ((leftres (equal-inf-step (first logic)))
                       ;; Compute right recursion if left gives no result.
                       (rightres (if (first leftres) nil
                                   (equal-inf-step (second logic)))))
                  (if (first leftres)
                    ;; If recursing left gets a result, combine inferred 
                    ;; left branch with right branch.
                    (list (first leftres) 
                          (propagate-polarity
                            (list (second leftres) (second logic))
                            :polarity polar))
                    ;; Combine left branch with inferred right branch.
                    (list (first rightres) 
                          (propagate-polarity
                            (list (car logic) (second rightres))
                            :polarity polar)))))))))))






;; Primitive inference step (to test inferences work at all...).
;; Returns (bool, formula) where the first argument is whether
;; an inference was made and the second argument is the formula
;; resulting from the inference.
(defun inf-step (f)
  ;; Just perform the first matching inference.  We don't have enough rules
  ;; right now for it to get stuck on a loop (most likely).  We'll start with
  ;; implicative inferences and back off to equality inferences to ensure that.
  (let (result)
    ;; First try to do an implicative inference.
    (setq result (impl-inf-step f))
    ;(format t "inf-step; result ~s~%~%" result)
    (if (first result)
      result
      ;; Then try equality inferences.
      (equal-inf-step f))))

