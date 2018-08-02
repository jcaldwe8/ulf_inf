;; Gene Kim 11/21/2017

(defparameter *debug* nil)

;(load "/p/nl/tools/ttt/src/load")
(load "../ulf_natural_logic/ttt/src/load.lisp")

;; TTT ULF patterns copied from sanity checker.
(load "../ulf_natural_logic/inference/ttt-lexical-patterns.lisp")
(load "../ulf_natural_logic/inference/ttt-phrase-patterns.lisp")

(load "../ulf_natural_logic/inference/util.lisp")
;;(load "polarity-util.lisp")
(load "../ulf_natural_logic/inference/ulf-macros.lisp")
(load "../ulf_natural_logic/inference/inference-rules.lisp")
(load "../ulf_natural_logic/inference/inf-engine.lisp")


(load "../ulf_natural_logic/inference/fracas-inf.lisp")

;; Testing.
(load "../ulf_natural_logic/inference/test/ulf-macro-test.lisp")
(load "../ulf_natural_logic/inference/test/ulf-normalize-test.lisp")

