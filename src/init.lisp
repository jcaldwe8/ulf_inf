
(setq *debug-ulf-inf* nil)

;; Dependencies.
(load "../ttt/src/load")

;; Utility functions.
(load "util.lisp")
(load "util-from-pilot-project.lisp")
(load "ttt-preds-and-functions.lisp")


(ql:quickload :cl-strings)
(ql:quickload :alexandria)
(setq *uppen-morph-filepath* 
      "../resources/uppen_morph_analysis/morph_english.sexp")
(setq *top10000-word-filepath*
      "../resources/google-10000-english.txt")
(load "uppen-morph.lisp")

;; Core inference code.
(load "inference-core.lisp")

;; Pheomenon-specific code.
(load "counterfactual-inferences.lisp")
(load "request-inferences.lisp")
(load "implicative-inferences.lisp")
(load "implicative-weak-inferences.lisp")

;; Top-level inference code.
(load "inference.lisp")

;; Examples.
(load "example/ttt-function-examples.lisp")

;; Dynamic polarity
;; GK: please comment this out if not used since it takes some time to load.
; (load-uppen-morph *uppen-morph-filepath*)
; (setq *dynamic-polarity-dir* "dynamic-polarity")
; (load "dynamic-polarity/dynamic-polarity.lisp")
