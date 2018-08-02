
(load "../ulf_natural_logic/inference/test/test-formulas.lisp")

(defun test-print (fn-name in &key (depolarize nil))
  (let (out)
    (format t "====TEST-PRINT====~%")
    (format t "Function:~%~s~%" fn-name)
    (format t "Input:~%~s~%" in)
    (setq out (funcall fn-name in))
    (format t "Output:~%~s~%" out)
    (if depolarize
      (progn (format t "Depolarized Input: ~s~%" (depolarize in))
             (format t "Depolarized Output: ~s~%" (depolarize out))))
    (format t "~%~%")))




(defun run-macro-tests (&key (depolarize nil))
  (test-print #'apply-ulf-macros fracas-test1p :depolarize depolarize)
  (test-print #'apply-ulf-macros fracas5pp :depolarize depolarize)
  (test-print #'apply-ulf-macros fracas5hp :depolarize depolarize)
  (test-print #'apply-ulf-macros fracas6pp :depolarize depolarize)
  (test-print #'apply-ulf-macros fracas6hp :depolarize depolarize)
  (test-print #'apply-ulf-macros fracas16pp :depolarize depolarize)
  (test-print #'apply-ulf-macros fracas16hp :depolarize depolarize))


