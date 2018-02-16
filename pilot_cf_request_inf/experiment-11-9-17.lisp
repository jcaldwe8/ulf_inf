
;;; read-entries-from-file
;;;
;;; Reads the struct definitions from file.
;;; Returns list of verb-entry structures.
(defun read-entries (file)
  (labels
    ((helper
       (in acc)
       (let ((entry (read in nil)))
         (if (null entry)
           acc
           (helper in (cons entry acc))))))
    (reverse (helper (open file) '()))))

;; Preprocess formula.
;; Remove { } symbols.
;; not -> not.adv-s
(defun preproc-formula (f)
  (cond
    ((equal f 'not) 'not.adv-s)
    ((atom f) f)
    (t (remove-if (lambda (x) (member x '({ })))
                  (mapcar #'preproc-formula f)))))

;; Read and preprocess formulas from file.
(defun get-formulas (file)
  (mapcar #'preproc-formula 
          (read-entries file)))

(load "init.lisp")

;; Selectively sampled.
(setq select (get-formulas "data/select-sampled-ulf.txt"))
;; Generally sampled.
(setq general (get-formulas "data/general-sampled-ulf.txt"))

(format T "Select Num ~s~%" (length select))
(format T "General Num ~s~%" (length general))

(setq select-output
      (results-from-applying-rules *rule-names* select nil))
(setq general-output
      (results-from-applying-rules *rule-names* general nil))

(format t "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv~%")
(format t "Output from selectively sampled formulas~%")
(format t "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^~%")
(format t "~s~%~%~%~%" select-output)


(format t "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv~%")
(format t "Output from generally sampled formulas~%")
(format t "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^~%")
(format t "~s~%~%~%~%" general-output)



