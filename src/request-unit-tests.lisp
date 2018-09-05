;; Gene Kim 8-24-2018
;;
;; Unit tests for request-inferences.lisp.


(defun want-or-should-from-neg-request-only (ulf)
  (result-formula
    (cadar (results-from-applying-rules 
             (list #'infer-want-or-should-from-neg-request) 
             (list ulf) t))))
(defun not-current-from-neg-request-only (ulf)
  (result-formula
    (cadar (results-from-applying-rules 
             (list #'infer-not-current-from-neg-request) 
             (list ulf) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for negative request inferences.
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test neg-request-basic-test
  "Basic tests for negative requests -- taken from the inference guidelines"
  (:tag :neg-req :basic)
  (let
    ((ex1 '(((pres Can.aux-v) not you.pro 
                              (be.v friendly.a)) ?))
     (ex2 '(((pres Could.aux-v) not you.pro 
                                (use.v (the.d (blue.a block.n)))) ?)))

    (assert-equal
      '((I.pro ((pres want.v) you.pro (to (be.v friendly.a))))
        or.cc
        (I.pro ((pres think.v) (tht (you.pro ((pres should.aux-v) 
                                              (be.v friendly.a)))))))
      (want-or-should-from-neg-request-only ex1))

    (assert-equal
      '((you.pro ((pres prog) not (be.v friendly.a)))
        or.cc
        (you.pro ((pres do.aux-s) not (plan.v (to (be.v friendly.a))))))
      (not-current-from-neg-request-only ex1))

    (assert-equal
      '((I.pro ((pres want.v) you.pro
                (to (use.v (the.d (blue.a block.n))))))
        or.cc
        (I.pro ((pres think.v) 
                (tht (you.pro ((pres should.aux-v) 
                               (use.v (the.d (blue.a block.n)))))))))
      (want-or-should-from-neg-request-only ex2))

    (assert-equal
      '((you.pro ((pres prog) not 
                  (use.v (the.d (blue.a block.n)))))
        or.cc
        (you.pro ((pres do.aux-s) not
                  (plan.v (to (use.v (the.d (blue.a block.n))))))))
      (not-current-from-neg-request-only ex2))))



