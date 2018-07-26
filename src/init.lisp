
(setq *debug-ulf-inf* nil)

;; Dependencies.
(load "../ttt/src/load")

;; Core inference code.
(load "util-from-pilot-project.lisp")
(load "ttt-preds-and-functions.lisp")
(load "inference.lisp")

;; Pheomenon-specific code.
(load "counterfactual-inferences.lisp")
(load "request-inferences.lisp")

;; Examples.
(load "example/ttt-function-examples.lisp")

