; Gene 7-29-2018
; Adjusted from Len's tests from sentences sent by Gene for the pilot
; experiments.  Duplicates with len-pilot-tests.lisp were removed, which only
; left two tests.
;
; Len's initial comments:
; > THIS WAS AN INITIAL ATTEMPT TO PROCESS GENE'S EXAMPLES, NOT ENTIRELY
; > SUCCESSFUL.
;==========================================================================


;; Macro for running a test using 'run-subset-rules' to reduce the size of the
;; test declarations.
;; Arguments
;;  name:     The name of the test (as a symbol, i.e. unquoted)
;;  sentence: String surface sentence being tested
;;  ulf:      Source ULF
;;  expected: List of expected output ULFs (in order TODO: make the order not
;;            matter)
(defmacro define-pilot-gene-devset-subset-test (name sentence tags ulf expected)
  `(define-test ,name
     ,(format nil "'infer-all' test on the sentence '~a'" sentence) 
     (:tag :subset-rules :gene-devset ,@tags)
     (let ((result (run-subset-rules ,ulf))
           (ulf ,ulf)
           (expected ,expected))
        (if expected
          (assert-equal (first result) ulf 
                        (first result) ulf))
        (list-assert-equal (cdr result) expected))))

;; Macro for running a test using 'infer-all' (from the core inference code).
;; Arguments
;;  name:     The name of the test (as a symbol, i.e. unquoted)
;;  sentence: String surface sentence being tested
;;  ulf:      Source ULF
;;  expected: List of expected output ULFs (in order TODO: make the order not
;;            matter)
(defmacro define-pilot-gene-devset-infer-all-test (name sentence tags ulf expected)
  `(define-test ,name
     ,(format nil "Subset rule test on the sentence '~a'" sentence) 
     (:tag :infer-all :gene-devset ,@tags)
     (let ((actual (mapcar #'result-formula (infer-all ,ulf)))
           (expected ,expected))
        (list-assert-equal actual expected))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subset tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-pilot-gene-devset-subset-test
  test-devset-subset-wish-16549
  "You will wish you had never seen it."
  (:wish :cf)
  '(you.pro ((pres will.aux-s) 
             (wish.v (tht (you.pro ((cf perf) never.adv-f (see.v it.pro)))))))
  '((you.pro ((past do.aux-s) (see.v it.pro)))))

(define-pilot-gene-devset-subset-test
  test-devset-subset-if-inv-neg
  "Had I not met her, I would be in Rome."
  (:ifinv :cf)
  '((((cf perf) i.pro not.adv-s (meet.v her.pro)) 
     (I.pro ((pres would.aux-s) (be.v (in.p |Rome|))))))
  '((i.pro ((past meet.v) her.pro))
    (i.pro ((pres be.v) not.adv-s (in.p |Rome|)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipeline tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-pilot-gene-devset-infer-all-test
  test-devset-infer-all-wish-16549
  "You will wish you had never seen it."
  (:wish :cf)
  '(you.pro ((pres will.aux-s) 
             (wish.v (tht (you.pro ((cf perf) never.adv-f (see.v it.pro)))))))
  '((you.pro ((past do.aux-s) (see.v it.pro)))))

(define-pilot-gene-devset-infer-all-test
  test-devset-infer-all-if-inv-neg
  "Had I not met her, I would be in Rome."
  (:ifinv :cf)
  '((((cf perf) i.pro not.adv-s (meet.v her.pro)) 
     (I.pro ((pres would.aux-s) (be.v (in.p |Rome|))))))
  '((i.pro ((past meet.v) her.pro))
    (i.pro ((pres be.v) not.adv-s (in.p |Rome|)))))

