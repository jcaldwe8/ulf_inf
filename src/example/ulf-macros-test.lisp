;;  Lisp unit tests for ulf macros

;; Load normal system code 
(load "../ulf_natural_logic/inference/init.lisp")


;;;(defun run-macro-tests (&key (depolarize nil))
;;;  (test-print #'apply-ulf-macros fracas-test1p :depolarize depolarize)
;;;  (test-print #'apply-ulf-macros fracas5pp :depolarize depolarize)
;;;  (test-print #'apply-ulf-macros fracas5hp :depolarize depolarize)
;;;  (test-print #'apply-ulf-macros fracas6pp :depolarize depolarize)
;;;  (test-print #'apply-ulf-macros fracas6hp :depolarize depolarize)
;;;  (test-print #'apply-ulf-macros fracas16pp :depolarize depolarize)
;;;  (test-print #'apply-ulf-macros fracas16hp :depolarize depolarize))



(ql:quickload :lisp-unit)
(use-package :lisp-unit)
(setq *print-failures* t)
(setq *print-summary* t) 

;; Testing.


(define-test test-fracas5pp
    (assert-equal t 
                 (equal?
                  '(((THE.D ((REALLY.ADV-A AMBITIOUS.A) (PLUR TENOR.N)))
                     ((PRES BE.V) ITALIAN.A))
                    \.)
                  (apply-ulf-macros fracas5pp))))

    

(define-test test-fracas5hp
    (assert-equal t
                  (equal?
                   '((THERE.PRO
                      ((PRES BE.V)
                       (N+PREDS ((REALLY.ADV-A AMBITIOUS.A) (PLUR TENOR.N))
                        (:L X7
                         (AND.CC (X7 ((PRES BE.V) ITALIAN.A)) (X7 PERSON.N))))))
                     \.)
                     (apply-ulf-macros fracas5hp))))
;;failed?
(define-test test-fracas6pp 
    (assert-equal t 
                  (equal? 
                   '(((NO.D ((REALLY.ADV-A GREAT.A) (PLUR TENOR.N)))
                      ((PRES BE.V) MODEST.A))
                     \.)
                   (apply-ulf-macros fracas6pp))))

(define-test test-fracas6hp
    (assert-equal t
                  (equal?
                   '((THERE.PRO
                      ((PRES BE.V)
                       (N+PREDS ((REALLY.ADV-A GREAT.A) (PLUR TENOR.N))
                        (:L X11
                         (AND.CC (X11 ((PRES BE.V) MODEST.A)) (X11 PERSON.N))))))
                     \.)
                   (apply-ulf-macros fracas6hp))))


(define-test test-fracas16pp 
    (assert-equal t
                  (equal? 
                   '((((NQUAN (AT_MOST.ADV-A TWO.A)) (PLUR TENOR.N))
                     ((PRES WILL.AUX-S)
                      (CONTRIBUTE.V (THEIR.D (PLUR FEE.N)) (ADV-A (TO.P (K CHARITY.N))))))
                     \.)
     (apply-ulf-macros fracas16pp))))


(define-test test-fracas16hp
  (assert-equal t 
                (equal?
                 '((THERE.PRO
                   ((PRES BE.V)
                     (N+PREDS (PLUR TENOR.N)
                      (:L X16
                       (AND.CC
                        (X16
                         ((PRES WILL.AUX-S)
                          (CONTRIBUTE.V (THEIR.D (PLUR FEE.N))
                           (ADV-A (TO.P (K CHARITY.N))))))
                            (X16 PERSON.N))))))
                   \.)
   (apply-ulf-macros fracas16hp))))




(defun test-equal? ()
    (setq tr1 '((THERE.PRO
                 ((PRES BE.V)
                  (N+PREDS (PLUR TENOR.N)
                           (:L X16
                            (AND.CC
                             (X16
                              ((PRES WILL.AUX-S)
                               (CONTRIBUTE.V (THEIR.D (PLUR FEE.N))
                                (ADV-A (TO.P (K CHARITY.N))))))
                              (X16 PERSON.N))))))
                \.))
 (setq tr2 '((THERE.PRO
             ((PRES BE.V)
             (N+PREDS (PLUR TENOR.N)
                     (:L X34
                     (AND.CC
                     (X34
                     ((PRES WILL.AUX-S)
                     (CONTRIBUTE.V (THEIR.D (PLUR FEE.N))
                    (ADV-A (TO.P (K CHARITY.N))))))
                (X34 PERSON.N))))))
 \.))
    (setq tr3 '((((NQUAN (AT_MOST.ADV-A TWO.A)) (PLUR TENOR.N))
  ((PRES WILL.AUX-S)
   (CONTRIBUTE.V (THEIR.D (PLUR FEE.N)) (ADV-A (TO.P (K CHARITY.N))))))
                \.))
    
    (print 
    (equal? tr1 tr2))
   (print (equal? tr1 tr3)))



;;; tested
(defun in-hash-table (key hash-table)
  (if (not (null (gethash key hash-table)))
      t))  

(defun equal? (tr1 tr2)
  (setq hash-table (make-hash-table))
  (equal?-helper tr1 tr2 hash-table))  


(defun equal?-helper (tr1 tr2 hash-table)
  (cond
   ;;base case
   ((and (atom tr1) (not (atom tr2))) nil)
   ;;base case2
   ((and (atom tr1) (atom tr2))
    (if (and (var? tr1) (var? tr2))
        (if (in-hash-table tr1 hash-table) 
            (equal (gethash tr1 hash-table) tr2)
            (setf (gethash tr1 hash-table) tr2))
       (equal tr1 tr2)))
   ;; recursion
  ((and (listp tr1) (listp tr2))
   ;;(and (equal?-helper (car tr1) (car tr2) hash-table) 
     ;;   (equal?-helper (cdr tr1) (cdr tr2) hash-table)))))
   (reduce #'(lambda (x y) (and x y)) (mapcar #'(lambda (x y) 
                    (equal?-helper x y hash-table)) tr1 tr2)))))

;;; tested
;;; x is an atom, NOT  a string like I kept thinking bc I have no brains
(defun var? (x)
  (setq symbols (split-into-atoms x))
    (and (equal "X" (first symbols)) 
         (all-are-numbers (mapcar #'parse-integer (cdr symbols)))))

;;; tested
;;checks if all atoms in a list are numbers
(defun all-are-numbers (lst)
    (cond
     ((null lst) t)
     ((numberp (car lst))
      (all-are-numbers (cdr lst)))))

;;; tested
;;splits atoms of a list into a list of strings
(defun split-into-atoms (atm) 
    (mapcar #'(lambda (ch) (format nil "~a" ch)) 
              (coerce (string atm) 'list)))     




















