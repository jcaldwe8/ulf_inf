;;  Lisp unit tests for ulf macros

;; Load normal system code 
(load "../ulf_natural_logic/inference/init.lisp")

;; TTT ULF patterns copied from sanity checker.
;;(load "../ulf_natural_logic/inference/ttt-lexical-patterns.lisp")
;;(load "../ulf_natural_logic/inference/ttt-phrase-patterns.lisp")

;;(load "../ulf_natural_logic/inference/util.lisp")
;;(load "polarity-util.lisp")
;;(load "../ulf_natural_logic/inference/ulf-macros.lisp")
;;(load "../ulf_natural_logic/inference/inference-rules.lisp")
;;(load "../ulf_natural_logic/inference/inf-engine.lisp")


;;(load "../ulf_natural_logic/inference/fracas-inf.lisp")

;;(load "../ulf_natural_logic/inference/ulf-macros.lisp")

;; Load testing library.
;; Please see the following for documentation on using lisp-unit 
;; (https://github.com/OdonataResearchLLC/lisp-unit/wiki).


;;(load "../ulf_natural_logic/inference/test/test-formulas.lisp")

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



;;(ql:quickload :lisp-unit)
;;(use-package :lisp-unit)
;;(setq *print-failures* t)
;(setq *print-summary* t) 

;; Testing.
;;(load "test/ulf-macro-test.lisp")
;;(load "test/ulf-normalize-test.lisp")

;;(define-test test-ulf-macros
  ;;  (apply-ulf-macros fracas-test1p :depolarize depolarize)
    
    ;;)