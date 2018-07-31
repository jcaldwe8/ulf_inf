;; Gene Kim 11/21/2017

(defparameter *debug* nil)

;(load "/p/nl/tools/ttt/src/load")
(load "../ttt/src/load")

;; TTT ULF patterns copied from sanity checker.
(load "ttt-lexical-patterns.lisp")
(load "ttt-phrase-patterns.lisp")

(load "util.lisp")
;;(load "polarity-util.lisp")
(load "ulf-macros.lisp")
(load "inference-rules.lisp")
(load "inf-engine.lisp")


(load "fracas-inf.lisp")

;; Testing.
(load "test/ulf-macro-test.lisp")
(load "test/ulf-normalize-test.lisp")

