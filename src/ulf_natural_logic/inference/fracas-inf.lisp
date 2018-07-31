;;; Gene Kim 11/20/2017
;;; Code to perform FraCaS-style tasks which take a number of premises and a 
;;; hypothesis and returns a conclusion about the hypothesis.

(defun and? (x) (member x '(and and.cc))) ; TODO: move this somewhere more appropriate.
(defun not? (x) (member x '(not not.adv-s)))


;; Simplifies the input conjunction.
;; 1. Flatten nested conjunctions.
;; 2. Remove conjunction operator for a single argument.
;; 3. (Incomplete) If one predicate is a hypernym of another, then remove the hypernym.
;;  Remove conjunction if there's only a single predicate left.
;; Preserves order of predicates.
(defun simplify-conj (pconj)
  ;; TODO: use WordNet and Lore data to get predicate hierarchies.
  ;;       ignore complex predicates for now...
  (let* ((conj (get-formula pconj))
         (p (get-polarity pconj))
         (pelems (remove-if #'polar-and? conj))
         (elems (mapcar #'get-formula pelems))
         (inner-cnjs (remove-if-not #'(lambda (x) 
                                        (pred-member #'polar-and? 
                                                     (get-formula x))) 
                                    pelems))
         (inner-non-cnjs (remove-if #'(lambda (x) 
                                        (pred-member #'polar-and? 
                                                     (get-formula x)))
                                    pelems)))
    (cond
      ;; Only 1 element.
      ((equal 1 (length pelems))
       (car pelems))
      ;; Nested conjunction.
      ((not (null inner-cnjs))
       (let* ((inner-cnj-elems (remove-if #'polar-and? 
                                          (flatten (mapcar #'get-formula inner-cnjs))))
              (lifted (append inner-non-cnjs inner-cnj-elems))
              (lifted-cnj (cons (car lifted) (cons 'and.cc (cdr lifted)))))
         ;; Recurse to handle more than one level of nesting.
         ;(format t "Nested cnj~%elems ~s~%lifted ~s~%inner-cnjs ~s~%inner-cnj-elems ~s~%~%~%" elems lifted inner-cnjs inner-cnj-elems)
         (simplify-conj (propagate-polarity lifted-cnj :polarity p))))
      ;; Otherwise nothing to do.
      (t pconj))))


;; Simplifies the conjunctions in the given formula.
;; 1. Flatten nested conjunctions
;; 2. Remove conjunction operator if there is only a single argument.
(defun simplify-conjs (pf)
  (let ((f (get-formula pf))
        (p (get-polarity pf)))
    (cond
      ((atom f) pf)
      ((pred-member #'polar-and? f)
       (let ((newf (get-formula (simplify-conj pf))))
         (polarize (mapcar #'simplify-conjs newf) p)))
      (t (polarize (mapcar #'simplify-conjs f) p)))))


;; Removes trivial lambdas 
;; (where the variable only occurs in subject position).
;; i.e. (:l <var> (<var> <pred>))
(defun simplify-lambda (pf)
  (if *debug* (format t "simplify-lambda; pf ~s~%~%" pf))
  (cond 
    ((polar-lambda? pf)
     (let* ((lamb (depolarize-lambda pf))
            (var (second lamb))
            (body (third lamb))
            (varcount (segment-count var body)))
       (if (and (equal varcount 1)
                (equal var (first (depolarize body :depth 2))))
         (second body)
         pf)))
    (t (format t "simplify-lambda called with not a lambda: ~s~%~%" f))))

;; Simplifies lambdas in a given formulas.
(defun simplify-lambdas (pf)
  (if *debug* (format t "simplify-lambdas; pf ~s~%~%" pf))
  (cond 
    ((atom (get-formula pf)) pf)
    ((polar-lambda? pf)
     (let ((simplified (simplify-lambda pf)))
       (polarize (mapcar #'simplify-lambdas (get-formula simplified))
                 (get-polarity simplified))))
    (t (polarize (mapcar #'simplify-lambdas (get-formula pf))
                 (get-polarity pf)))))


;; Lexical determiner equvialence classes.
(defparameter *lexdet-classes*
  '((some.d a.d an.d some a an)
    (every.d each.d every each)
    (many.d many)
    (few.d few)
    (all.d all)
    (no.d no)
    (both.d both)
    (neither.d neither)
    (the.d the)))
(defun polar-lex-det? (pf)
  (lex-det? (depolarize pf :depth 1)))

;; Normalizes the ULF.
;; First apply ULF macros.
;; Then apply simplification inferences:
;;  Remove redundant predicates in conjunctive phrases
;;  Remove trivial lambdas
;;  Remove double negations
;;  Replace lexical determiners with canonical equivalent, e.g. a.d -> some.d
;; TODO: decide if we want to normalize lexical items, 
;;       e.g. and.cc -> and; not.adv-s -> not
(defun normalize-ulf (ulf)
  (if *debug* (format t "normalize-ulf; ulf ~s~%~%" ulf))
  (labels 
    (
     ;; Test for double-neg with extra conditions to avoid exceptions.
     (doubleneg? (pf)
       (if *debug* (format t "doubleneg?; pf ~s~%~%" pf))
       (let ((f (depolarize pf)))
         (and (listp f)
               (equal 2 (length f))
               (not? (first f))
               (listp (second f))
               (equal 2 (length (second f)))
               (not? (first (second f))))))
     ;; Removes double negations.
     (remove-doublenegs (pf)
       (if *debug* (format t "remove-doublenegs; pf ~s~%~%" pf))
       (cond 
         ((atom (get-formula pf)) pf)
         ((doubleneg? pf)
          (remove-doublenegs (second (get-formula 
                                       (second (get-formula pf))))))
         (t (polarize (mapcar #'remove-doublenegs (get-formula pf)) 
                      (get-polarity pf)))))
     ;; Normalize lexical determiners.
     (normalize-lexdets (pf)
       (if *debug* (format t "normalize-lexdets; pf ~s~%~%" pf))
       (cond
         ((polar-lex-det? pf)
          (let* ((det (get-formula pf))
                 (normdet
                   (reduce #'(lambda (acc dclass)
                               (if (member acc dclass) 
                                 (first dclass)
                                 acc))
                           *lexdet-classes* :initial-value det)))
            (polarize normdet (get-polarity pf))))
         ((atom (get-formula pf)) pf)
         (t (polarize (mapcar #'normalize-lexdets (get-formula pf))
                      (get-polarity pf)))))
     ) ; end labels defs
  ;; Main body.
  (let* ((ulf1 (apply-ulf-macros ulf))
         (ulf2 (simplify-conjs ulf1))
         (ulf3 (simplify-lambdas ulf2))
         (ulf4 (remove-doublenegs ulf3))
         (ulf5 (normalize-lexdets ulf4)))
    ulf5)))


;; Runs inference and prints intermediate results.
(defun inference-print-loop (f)
  (labels
    (
     ;; Recurses while keeping track of previous formulas to avoid infinite
     ;; looping.
     (helper (f rule acc)
       (format t "Formula : ~s~%Rule : ~s~%^^^^^^^^^~%" f rule)
       (cond
         ((member f acc :test #'equal)
          (format t "Reached a formula twice, so stopping.")
          acc)
         (t 
           (let* ((infres (inf-step f))
                  (rule (if infres (first infres)))
                  (fnew (if infres (second infres))))
             (if (and rule
                      (not (equal f fnew)))
               (helper fnew rule (cons f acc))
               acc))))))
    (helper f nil nil)))


;; Takes a list of premises and a hypothesis and returns t, nil, or 'unknown.
;; First normalizes the premises and hypothesis (run ULF macros and simplify
;; formulas as much as possible).  The function also takes an optional argument
;; of max-steps to determine a limit on inferences before returning 'unknown.
;;
;; This does a slightly guided search, where if a premise very similar to the
;; hypothesis is identified, then inference is focused on that premise for a
;; few steps. If the hypothesis is still not proven correct or incorrect, then
;; full inference is returned.  
(defun fracas-run (ps h &optional max-steps)
  (let* ((nps (mapcar #'normalize-ulf ps))
         (nh (normalize-ulf h))
         (kb nps)
    ;; TODO: generalize to changing kb and multiple premises.  Currently just
    ;; getting inferences working on a single formula.
         (f (first kb)))
    (inference-print-loop f)))





  



