;; Gene Kim 7-24-2018

;; Load testing library.
(ql:quickload :lisp-unit)

;; Load normal system code.
(load "init")

;; Load testing code.
(load "test/test-util.lisp")
(load "test/pilot/test-ulfs.lisp")
; NB: these files run tests automatically, so comment out for now.
;(load "test/pilot/tests-gene.lisp")
;(load "test/pilot/tests-preliminary.lisp")

;; For checking proper initial system configuration.
;; TODO: remove once proper tests have been written.
(defparameter *rule-names* 
 '(infer-want-from-request infer-expect-from-request 
   infer-falsehood-from-positive-counterfactual 
   infer-falsehood-from-inverted-positive-counterfactual
   infer-fact-from-negative-counterfactual
   infer-fact-from-inverted-negative-counterfactual
 ))
(defun quickcheck ()
  (setq output (results-from-applying-rules *rule-names* *test-ulfs* t))
  (format t "~s" output))


