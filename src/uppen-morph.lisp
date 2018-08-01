;;; Gene Kim 8-1-2018
;;;
;;; Code for interacting with the Uppen Morphological Data.  It interfaces from
;;; a version of the file that has been formatted into S-expressions.  See
;;; resources/uppen_morph_analysis/.

(defparameter *uppen-morph-data* nil)

;; Loads the Uppen Morphological Data file provided, parses it into an
;; appropriate hashtable format and stores it in *uppen-morph-data*.
;;
;; Data format:
;;  (<token> ((<lemma> <POS> ([list of features])) ...)
;; Where features can be: "3sg", "PPART", "GEN", etc.
;; e.g.
;;  (run ((run N (3sg)) (run V (PPART STR)) (run V (INF))))
;;
;; The stored hashtable will have the structure:
;;  <lemma> -> ((<token> <POS> ([list of features])) ...)
;;
(defun load-uppen-morph (sexp-filepath)
  (print "Loading Uppen Morph data...")
  (let ((rawentries (read-file-objects2 sexp-filepath))
        (ht (make-hash-table :test #'equal))
        entries)
    ;; Reverse the entries: <lemma> -> (<token> <POS> ([features]))
    (dolist (e rawentries)
      ;(print e)
      (let* ((token (car e))
             (lemma (caaadr e))
             (newentry (cons token (cdaadr e))))
        (setf (gethash lemma ht)
              (cons newentry (gethash lemma ht)))))
    (setq *uppen-morph-data* ht))
  (print "Done!"))

;; Conjugates the given lemma and POS into the construction with the given
;; features.  If none are found, it tries to guess.  If results are found, 
;; the lemma is simple returned.
(defun um-conjugate (lemma pos features)
  (if (not *uppen-morph-data*)
    (error "Please first call 'load-uppen-morph' to set up the data structures."))
  (let ((cnjs (gethash lemma *uppen-morph-data*))
        poscnjs filtered)
    (setq poscnjs (remove-if-not #'(lambda (x) (equal pos (second x))) cnjs))
    (if (not poscnjs) (return-from um-conjugate lemma))
    (setq sorted (sort poscnjs 
                       #'(lambda (x y)
                           (> (length (intersection (third x) features))
                              (length (intersection (third y) features))))))
    (caar sorted)))





