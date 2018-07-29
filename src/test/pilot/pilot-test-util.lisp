;;; Gene Kim 7-29-2018
;;; Code to hold shared parameters and functions.

;; Define applicable rule subset for request and counterfactuals that we want
;; to test.
(defparameter *rule-names* 
 '(infer-want-from-request-raw infer-expect-from-request-raw 
   infer-falsehood-from-positive-counterfactual-raw
   infer-falsehood-from-inverted-positive-counterfactual-raw
   infer-fact-from-negative-counterfactual-raw
   infer-fact-from-inverted-negative-counterfactual-raw
 ))
;; Run the selected subset of rules on the given ULF and return a list of:
;; (ulf result1 result2 ...)
(defun run-subset-rules (ulf)
  (car (results-from-applying-rules *rule-names* (list ulf) t)))

;; Helper function to assert equality between lists, one by one.
;; TODO: make the exact assert function an argument for generalizability.
(defun list-assert-equal (actual expect)
  (assert-equal (length expect) (length actual) 
                (length expect) (length actual) 
                expect actual)
  (mapcar #'(lambda (x) 
              (let ((expect (first x))
                    (actual (second x)))
                (assert-equal expect actual 
                              expect actual)))
          (mapcar #'list expect actual)))

