;; Gene Kim 12/10/2017
;; Polarity utility functions.

;; Polarities: positive, negative, neither, respectively.
(defparameter *polarities* '(+ - \#))

;; Propagate polarity directly if missing.
;; Key argument :polarity is only used if the formula is not polarized.
(defun propagate-polarity (pfin &key (polarity '+))
  (if *debug* (format t "propagate-polarity: ~s~%~%" pfin))
  (labels
    (
     ;; Heavy lifter.
     (helper (pf p)
       (cond
         ((null pf) pf)
         ((polarized? pf) 
          (let ((pnew (get-polarity pf))
                (f (get-formula pf)))
            (if (atom f)
              pf
              (polarize (mapcar #'(lambda (x) (helper x pnew)) f)
                        pnew))))
         ((atom pf) (polarize pf p))
         (t (polarize (mapcar #'(lambda (x) (helper x p)) pf)
                      p)))))
    (if (polarized? pfin)
      (helper pfin (get-polarity pfin))
      (helper pfin '+))))

(defun polarize (f p) (list p f))
(defun polarized? (pf)
  (and (listp pf)
       (= 2 (length pf))
       (polar? (first pf))))
(defun polar-emb-op? (pf)
   (emb-op? (depolarize pf)))

;; Lambdas have transparent polarity, so it's easy to remove and add the
;; polarity.
;; TODO: add checks that pf is in fact a lambda.
(defun depolarize-lambda (pf)
  (depolarize pf :depth 2))
(defun repolarize-lambda (f p)
  (polarize 
    (mapcar #'(lambda (x) (polarize x p)) f) p))

(defun polar? (x) (member x *polarities*))

;; Depolarizes a formula one level at a time. Simply returns unpolarized 
;; formulas unchanges.
(defun depolarize (pf &key (depth -1 depth-supplied-p))
  (cond
    ((equal depth 0) pf)
    ((atom pf) pf)
    ((and (polar? (first pf))
          (atom (second pf))) (second pf))
    ((and (polar? (first pf))
          (listp (second pf)))
     (mapcar #'(lambda (x) 
                 (depolarize x :depth (max -1 (- depth 1)))) 
             (second pf)))
    ((listp pf) 
     (mapcar #'(lambda (x)
                 (depolarize x :depth (max -1 (- depth 1))))
             pf))
    (t (format t "ERROR: unknown format for depolarize: ~s~%" pf)))) 

;; Extracts the polarity from a logical segment.
;; TODO: use struct to represent polarity marked logical segment.
(defun extract-polarity (pf)
  (cond
    ((polarized? pf) (first pf))
    (t "ERROR: bad input to extract-polarity: ~s~%~%" pf)))

;; Extracts segment pfrom polarity marked logical segment.
(defun extract-logic (pf)
  (cond
    ((polarized? pf) (second pf))
    (t "ERROR: bad input to extract-polarity: ~s~%~%" pf)))

;; Aliases for extract-polarity and extract-logic.
(defun get-polarity (pf) (extract-polarity pf))
(defun get-formula (pf) (extract-logic pf))


;; Polarized variants of lexical identification functions.
(defun polar-noun? (pf)
  (let ((f (depolarize pf)))
    (noun? f)))

(defun polar-and? (x) (and? (depolarize x :depth 1)))
(defun polar-not? (x) (not? (depolarize x :depth 1)))


