
(setq *debug-ulf-inf* nil)

;; Dependencies.
(load "../ttt/src/load")

;; Utility functions.
(load "util-from-pilot-project.lisp")
(load "ttt-preds-and-functions.lisp")

;; Core inference code.
(load "inference-core.lisp")

;; Pheomenon-specific code.
(load "counterfactual-inferences.lisp")
(load "request-inferences.lisp")

;; Top-level inference code.
(load "inference.lisp")

;; Examples.
(load "example/ttt-function-examples.lisp")

