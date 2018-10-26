;; Gene Kim 7-24-2018

;; Load testing library.
;; Please see the following for documentation on using lisp-unit 
;; (https://github.com/OdonataResearchLLC/lisp-unit/wiki).
(ql:quickload :lisp-unit)
(use-package :lisp-unit)

;; Lisp unit printing options.
(setq *print-failures* t)
;(setq *print-summary* t)

;; Local printing options.
(setq *debug-ulf-inf* nil)

;; Load normal system code.
(load "init")

;; Load testing code.
(load "test/test-util.lisp")
;; (load "example/lisp-unit-example.lisp")
(load "test/pilot/pilot-test-util.lisp")
(load "test/pilot/len-pilot-tests.lisp")
(load "test/pilot/gene-devset-tests.lisp")
(load "test/pilot/select-sampled-ulf.lisp")
(load "test/pilot/general-sampled-ulf.lisp")
(load "test/counterfactual-unit-tests.lisp")
(load "test/implicative-unit-tests.lisp")
(load "test/ttt-preds-and-functions-tests.lisp")
; NB: these files run tests automatically, so comment out for now.
;(load "test/pilot/tests-gene.lisp")
;(load "test/pilot/tests-preliminary.lisp")


;; October 2018 evaluation tests.
(ql:quickload :cl-strings)
(ql:quickload :uiop)
(load "test/oct-2018-inference-eval/data-load.lisp")
(load "test/oct-2018-inference-eval/run-eval.lisp")

