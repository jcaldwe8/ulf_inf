;; Gene Kim 7-24-2018

;; Load testing library.
;; Please see the following for documentation on using lisp-unit 
;; (https://github.com/OdonataResearchLLC/lisp-unit/wiki).
(ql:quickload :lisp-unit)
(use-package :lisp-unit)
(setq *print-failures* t)
;(setq *print-summary* t)

;; Load normal system code.
(load "init")

;; Load testing code.
(load "test/test-util.lisp")
(load "test/pilot/len-pilot-tests.lisp")
; NB: these files run tests automatically, so comment out for now.
;(load "test/pilot/tests-gene.lisp")
;(load "test/pilot/tests-preliminary.lisp")
