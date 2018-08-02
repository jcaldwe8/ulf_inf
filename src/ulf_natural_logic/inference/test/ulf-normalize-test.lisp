
(load "../ulf_natural_logic/inference/test/test-formulas.lisp")

(setq lex-det-tests
      (list 
        '(many men.n)
        '(an.d animal.n)
        '(an animal.n)
        '(a person.n)
        '(a.d person.n)
        '(some animal.n)
        '(some.d animal.n)
        '(every.d animal.n)
        '(every animal.n)
        '(each.d animal.n)
        '(each animal.n)
        '(many.d men.n)
        '(neither animal.n)
        '(neither.d animal.n)))

(setq fracas-prefix-tests
      (list 
        fracas-test0
        fracas-test1
        fracas5p
        fracas5h
        fracas6p
        fracas6h
        fracas16p
        fracas16h))

(setq double-neg-tests
      (list 
        '(not (not (|John| happy.a)))
        '(not.adv-s (not (|John| happy.a)))
        '(not (not.adv-s (|John| happy.a)))
        '(not.adv-s (not.adv-s (|John| happy.a)))
        '(not (|John| happy.a))
        '(not.adv-s (|John| happy.a))
        '(I.pro ((pres belive.v) (tht (not (not (|John| happy.a))))))))

(setq there-be-tests
      (list 
        '(There.pro ((pres be.v) (n+preds man.n happy.a excited.a)))))


(defun run-test-prints (fn-name inputs &key (depolarize nil))
  (mapcar #'(lambda (x) (test-print #'normalize-ulf x :depolarize depolarize))
          inputs))

(defun run-simplify-conj-tests ()
  (run-test-prints #'simplify-conjs 
                   (mapcar #'apply-ulf-macros 
                           (mapcar #'all-positive
                                   fracas-prefix-tests))
                   :depolarize t))

(defun run-fracas-normalize-tests (&key (depolarize nil))
  (run-test-prints #'normalize-ulf 
                   (mapcar #'all-positive fracas-prefix-tests) 
                   :depolarize depolarize)
  ;; Lexical determiner normalization.
  (run-test-prints #'normalize-ulf 
                   (mapcar #'all-positive lex-det-tests) 
                   :depolarize depolarize)
  ;; Double negation.
  (run-test-prints #'normalize-ulf 
                   (mapcar #'all-positive double-neg-tests)
                   :depolarize depolarize)
  ;; Test there-be constructions with multiple predicates in n+preds.
  (run-test-prints #'normalize-ulf 
                   (mapcar #'all-positive there-be-tests) 
                   :depolarize depolarize))

