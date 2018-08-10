;;; Gene Kim 11/20/2017
;;; Polarity-removed vr. 
;;; TO-DO: testing for n+preds and stuff, move it to the main testing function
;;; Functions and rules for applying ULF macros.
;;; 
;;; Currently implemented (some limitations noted in TODOs):
;;;     n+preds, np+preds
;;;     relative clauses
;;;     "There be"
;;;
;;; To be added:
;;;     possessives
;;;     type-shifters
;;;     [etc, need to go through annotation guidelines for full list.  
;;;      They're probably not all necessary for the FraCaS subset we're 
;;;      considering.]
;;;

;; Variable generation.
;; TODO: maybe move this somewhere more global...
(setq *ulf-var-state* 1)
(setq *ulf-var-base* 'x)


;; Return new variable and increment state.
;;what does state do?
(defun genvar ()
  (let ((retval (read-from-string 
                 (format nil "~s~s" *ulf-var-base* 
                         *ulf-var-state*))))
    (setq *ulf-var-state* (+ *ulf-var-state* 1))
    retval))



;; Simple macros.
;; Macros that can actually be handled with TTT (minimal contextual info 
;; needed).
(defparameter *ulf-simple-macro-ttt*
    nil)

;;
;; n+preds and np+preds.
;;
;; n+preds:
;; (n+preds <noun> <pred1> ... <predn>)
;; -> (lambda <var>
;;            (and.cc (<var> <noun>) (<var> <pred1>) ... (<var> <predn>)))
(defun n+preds (f)

  (cond
   ;; Bad application.
   ((or
     (null f)
     (> 2 (length f))
     (not (equal (first f) 'n+preds)))
    ;; TODO: reintroduce below after improving noun? and pred? coverage.
                                        ;(not (noun? (second f)))
                                        ;(not (null (remove-if #'pred? (cddr f)))))
    (format t "~%ERROR: n+preds function called on ~s~%~%" f))
   ;; Main body.
   (t
    (let ((var (genvar)))
      (list ':l var
            (cons 'and.cc
                  (mapcar #'(lambda (x) (list var x))
                          (cdr f))))))))

;;check if is n+preds
(defun n+preds? (f)
  (equal 'n+preds (first f)))

;;check if is np+preds
(defun np+preds? (f)
  (equal 'np+preds (first f)))

;; np+preds:
;; (np+preds <np> <pred1> ... <predn>)
;; -> (the.d 
;;      (lambda <var>
;;              (and.cc (<var> = <np>) (<var> <pred1>) ... (<var> <predn>))))
(defun np+preds (f)
  
  (cond
   ;; Bad application.
   ((or
     (null f)
     (> 2 (length f))
     (not (equal (first f)) 'np+preds) 
     (not (np? (second f)))
     (not (null (remove-if #'pred? (cddr f)))))
    (format t "ERROR: np+preds function called on ~s~%~%" f))
   ;; Main body.
   (t
    (let ((var (genvar)))
      (list 'the.d 
            (list ':l var
                  (cons 'and.cc
                        (list var '= (second f))
                        (mapcar #'(lambda (x) (list var x))
                                (cddr f)))))))))

;; Applies n+preds and np+preds macros to a formula.
(defun apply-x-preds (f)
  (cond
   ((atom f) f)
   ((n+preds? f)
    (n+preds (mapcar #'apply-x-preds f) ))
   ((np+preds? f)
    (np+preds (mapcar #'apply-x-preds f)))
   (t (mapcar #'apply-x-preds f))))

;;
;; "There be"
;;

;; "There are ..." construction
(defparameter *there-be-ttt*
    '(
      (/ (There.pro (((!1 tense?) be.v) (!2 noun?)))
         ((some.d !2) (!1 exist.v)))
      (/ (There.pro (((!1 tense?) be.v) 
                     (:l _!2 ((_!2 (!3 noun?)) and?
                                               (_!2 _!4)))))
       ((some.d !3) _!4))))

;; TODO: recurse into full formula.
(defun handle-there-be (f)
  (ttt:apply-rules *there-be-ttt* f :rule-order :earliest-first :max-n 1))

;;
;; Relative clauses.
;;

;; Function processes a single relative clause.
;; Assumes that it is processing the first occurrence of .rel (using dfs)
;; without recursing into more embedded sentences.
(defun proc-rel (f)
  (labels
      (
       ;; Combines helper results.
       (combine-hres
           (res1 res2)
         (list (cons (first res1)  (first res2))
               (if   (second res1) (second res1) (second res2))
               (if   (third res1)  (third res1)  (third res2))))
       ;; Returns the formulas with the first occurrence of .rel replaced with
       ;; appropriate mapping with the relativizer that was found and the outer
       ;; level variable for the relativizer.
       (helper
           (fm)
         (cond 
          ((null fm) (list fm nil nil))
          ((atom fm) 
           (if (lex-rel? fm) 
               (let ((var1 (genvar)) 
                     var2)
                 (list (case fm 
                         ((that.rel tht.rel which.rel) var1)
                         (who.rel 
                          (setq var2 (genvar))
                          (list 'the.d 
                                (list ':l var2 
                                      (list (list var2 'person.n) 'and.cc
                                            (list var2 '= var1))))))
                       fm var1))
             (list fm nil nil)))
          ((emb-op? (car fm))
           (let ((recres (mapcar #'proc-rels (cdr fm))))
             (format t "In emb-op? fm: ~s~%recres: ~s~%~%" fm recres)
             (list
              (cons (car fm)
                    (mapcar #'proc-rels (cdr fm)))
              nil nil)))                ; reset local vars when recursing into embedding ops.
          (t (let ((res (helper (car fm))))
               (if (second res)         ; found the relativizer, so stop recursing.
                   (combine-hres res (list (cdr fm) nil nil))
                 (combine-hres res (helper (cdr fm))))))))
       )                                ; end of labels defs.

    ;; Main body.
    (let* ((hres (helper f))
           (newf (first hres))
           (rlzr (second hres))
           (var1 (third hres)))
      ;; If a relativizer was found, wrap in lambda, otherwise just return the
      ;; original formula.
      (if rlzr
          (values t (list ':l var1 newf))
        (values nil newf)))))

;; Simplifications to relative clauses.
;; The directly expanded macros can be hard to work with so this function
;; performs simplifications that are logically equivalent, but more amenable to
;; automatic inference.
(defun simplify-rels (f)
  ;; TODO: add more simplifications..
  (let* ((def-expanded (expand-certain-definites f))
         (lamb-compressed (compress-lambda-chains def-expanded)))
    lamb-compressed))

;; Convert S[(the.d (:l var1 (and.cc (var1 = expr) ...)))]
;; to (and.cc S[expr] ...[var1 <- expr])
;; So if there's a definite expression where the predicate includes an
;; equality, then we substitute the equaivalent expression for the 
;; definite expression and the additional constraints are conjoined to
;; this new expression.
(defun is-simple-equality (f)
  (and (listp f)
       (= 3 (length f))
       (equal '= (second f))))
(defun is-simple-equality-with-expr (f expr)
  (and (is-simple-equality f)
       (member expr f)))
(defun is-certain-definite? (f)
  (and
   (listp f)
   (= 2 (length f))
   (member (first f) '(the the.d))      ; first arg is 'the.d
   (listp (second f))
   (= 3 (length (second f))) 
   (equal (first (second f)) ':l)       ; second arg is a lambda
   (let ((var (second (second f)))
         (body (third (second f))))
     (and
      (listp body)
      (< 1 (length body))
      ;; Body of lambda is a conjunction.
      (or (member 'and body) (member 'and.cc body))
      ;; Conjunction contains an equality with the var.
      (not (null (remove-if-not 
                  #'(lambda (x)
                      (is-simple-equality-with-expr x var))
                  body)))))))
;; Expands the certain definites in a formula.
(defun expand-certain-definites (f)
  (labels 
      (
       ;; Extracts the modified certain definite and additional clauses and
       ;; Returns a pair of the expression that is equivalent and the new
       ;; additional expressions.
       ;; Assumes the input is already a certain definite (doesn't check the
       ;; input).
       (extract (x)
         (let* ((lamb (second x))
                (var (second lamb))
                (body (third lamb))
                (var-eq-exprs (remove-if-not 
                               #'(lambda (y) 
                                   (is-simple-equality-with-expr y var))
                               body))
                ;; TODO: generalize to handle all cases (for not assume that
                ;; just taking the first is sufficient)
                (var-eq-expr (first var-eq-exprs))
                (eqexpr (first (remove-if #'(lambda (y) 
                                              (member y (list var '=)))
                                          var-eq-expr)))
                ;; Filter and/and.cc and the equality that is being applied.
                (filteredbody 
                 (remove-if 
                  #'(lambda (y) 
                      (or (member y '(and and.cc))
                          (and (is-simple-equality-with-expr y var)
                               (is-simple-equality-with-expr y eqexpr))))
                  body))
                (newbodyexprs (subst eqexpr var filteredbody)))
           (list eqexpr newbodyexprs)))
       ;; Recursively expands certain definites and pieces together the 
       ;; conjoined expressions at the sentence embeddings.
       ;; Returns (new-formula conjoined-exprs)
       (recurser (x)
         (cond
          ((null x) (list nil nil))
          ((atom x) (list x nil))
          ((is-certain-definite? x) (extract x))
          ;; Call the full function (that integrate the extracted expressions
          ;; before returning) since the conjunctions shouldn't percolate 
          ;; through embedding operators.
          ((emb-op? (car x))
           (list (cons (car x) (mapcar #'expand-certain-definites (cdr x)))
                 nil))
          ;; Recurse and combine results.
          (t (let* ((carres   (recurser (car x)))
                    (carnewf  (first carres))
                    (carexprs (second carres))
                    (cdrres   (recurser (cdr x)))
                    (cdrnewf  (first cdrres))
                    (cdrexprs (second cdrres)))
               (list (cons carnewf cdrnewf)
                     (append carexprs cdrexprs))))))
       )                                ; end of labels defs.
    ;; Main body.
    ;; Here we rebuild the mapped formula.
    (let* ((recres (recurser f))
           (newf (first recres))
           (cnjexprs (second recres)))
      (if cnjexprs
          (cons 'and.cc (cons newf cnjexprs))
        newf))))



;; Compress chained lambda expressions.
(defun is-lambda (f)
  (and 
   (listp f)
   (= 3 (length f))
   (equal ':l (first f))))
(defun compress-lambda-chains (f)
  (labels
      (
       ;; Searches x for a lambda expression applied to var1 and unchains it by
       ;; replacing the lambda with the inner expression substituting var1 for
       ;; the inner lambda variable.
       (unchain-inner-lambda (x var1)
         (cond
          ((and (listp x)
                (= 2 (length x))
                (equal (first x) var1)
                (is-lambda (second x)))
           (let ((var2 (second (second x)))
                 (body (third (second x))))
             (subst var1 var2 body)))
          ((atom x) x)
          (t (let ((carres (unchain-inner-lambda (car x) var1))
                   (cdrres (unchain-inner-lambda (cdr x) var1)))
               (cons carres cdrres)))))
       
       ;; If a chained lambda expression, returns the unchained version.
       ;; Otherwise returns nil.
       (unchain-lambda (x)
         (cond
          ((is-lambda x)
           (let* ((var1 (second x))
                  (unchained (unchain-inner-lambda (third x) var1)))
             (if (equal unchained (third x))
                 nil
               unchained)))
          (t nil)))
       )                                ; end of labels defs.

    ;; Main body of compress-lambda-chains.
    (cond
     ((is-lambda f) 
      (let ((unchained (unchain-lambda f)))
        ;; If unchaining gives a result, then try again.
        ;; Otherwise, we've exhausted this lambda for unchaining.
        (if unchained
            (compress-lambda-chains unchained)
          f)))
     ;; Base case, just return.
     ((atom f) f)
     ;; Recursive case.
     (t (mapcar #'compress-lambda-chains f)))))




;; Processes all relative clauses in a sentence.
;; TODO: recurse into embedded sentences.
;; Keep track of most recent sentence embedding while recursing.
(defun proc-rels (f &key (simplify nil)) 
  (multiple-value-bind (res f1) (proc-rel f)
    (let ((raw-procd (if res (proc-rels f1) f1)))
      (if simplify 
          (simplify-rels raw-procd)
        raw-procd))))


;; Main function to apply macros.
;; This applies all the implemented macros in the necessary order.
(defun apply-ulf-macros (ulf)

  (let* ((ulf1 (proc-rels ulf :simplify
                          t))
         (ulf2 (apply-x-preds
                ulf1))
        (ulf3 (handle-there-be ulf2)))
    ulf1))

;;(apply-x-preds (proc-rels fracas5h :simplify t))










