
;; Requires asdf and quicklisp
;; https://www.quicklisp.org/

(ql:quickload :cl-strings)
(ql:quickload :alexandria)

(setq *debug-ulf-inf* nil)

(load "../../ttt/src/load.lisp")

;; Utility functions.
(load "../util.lisp")
(load "../util-from-pilot-project.lisp")
(load "../ttt-preds-and-functions.lisp")

(setq *uppen-morph-filepath*
      "../../resources/uppen_morph_analysis/morph_english.sexp")
(load "../uppen-morph.lisp")
(load-uppen-morph *uppen-morph-filepath*)

(setq *dynamic-polarity-dir* ".")
(load "dynamic-polarity.lisp")

