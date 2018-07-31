;;; Gene Kim 11/20/2017
;;; Inference rule definitions and related functions.

;; Rule structures:
;;  Equality rule:  (= <left elem> <right elem>)
;;  Inference rule: (/ <antecedent> <consequent>)
;; All angle bracketed parts can be TTT patterns.
;; NB: hmmm... there seems to be an issue that not all TTT patterns are
;; bi-directional.  We might want to create separate lists for unidirectional
;; and bidirectional patterns.

(defun equal-left-elem (rule)
  (assert (equal '= (first rule)))
  (second rule))
(defun equal-right-elem (rule)
  (assert (equal '= (first rule)))
  (third rule))
;; Reverse the order of an equality rule.
(defun reverse-equal-rule (rule)
  (assert (equal '= (first rule)))
  (list '= (third rule) (second rule)))

;; Reverse the order of an implicative rule.
(defun reverse-impl-rule (rule)
  (assert (equal '/ (first rule)))
  (list '/ (third rule) (second rule)))

(defun rule-antecedent (rule)
  (assert (equal '/ (first rule)))
  (second rule))
  ;(list '/ (second rule)))
(defun rule-consequent (rule)
  (assert (equal '/ (first rule)))
  (third rule))
  ;(list '/ (third rule)))

;; Quantifier equalities.
(defparameter *quantifier-equals*
  (list 
    '(= ((no.d _!1) _+2) 
        (not ((some.d _!1) _+2)))
    '(= ((some.d _!1) _+2) 
        (not ((no.d _!1) _+2)))
    '(= some.d a.d)
    '(= a.d an.d)
    '(= some.d an.d)
    '(= each.d every.d)
))
;; Variant of *quantifier-equals* where the expressions are polarized.
;; Consequent expressions have incomplete polarity, since only the top
;; level and polarity flips need to be marked.  This means these rules
;; aren't symmetric so we need two rules for any that use that property.
(defparameter *polar-quantifier-equals*
  (list 
    ;; ((no x) y) <-> (not ((some x) y))
    '(= ((!3 polar?) ((polar? ((polar? no.d) _!1)) 
                      _+2)) 
       (!3 (not ((polaropp !3) ((some.d _!1) _+2)))))
    '(= ((!3 polar?) ((polar? not)
                      (polar? ((polar? ((polar? some.d) _!1)) _+2))))
        (!3 ((!3 ((!3 no.d) _!1)) _+2)))

    ;; ((some x) y) <-> (not ((no x) y))
    '(= ((!3 polar?) 
         ((polar? ((polar? some.d) _!1)) _+2))

        (!3 ((!3 not) 
             ((polaropp !3) 
              (((polaropp !3) 
                (((polaropp !3) no.d) _!1)) _+2)))))

    '(= some.d a.d)
    '(= a.d an.d)
    '(= some.d an.d)
    '(= each.d every.d)
))

(defparameter *polarity-flip*
  '((/ (polaropp +) -)
    (/ (polaropp -) +)
    (/ (polaropp \#) \#)))

;; Quantifier unidirectional equalities.
(defparameter *quantifier-unidir-equals*
  (list 
    '(= (every.d (!1 singular-noun?))
        (all.d (plur !1)))))

;; Unidirectional inferences in positive contexts.
(defparameter *quantifier-infs*
  (list 
    '(/ all.d some.d)
    '(/ every.d some.d)
    '(/ many.d some.d)
    '(/ few.d some.d)
    '(/ the.d some.d)))
    
;; Equality inferences.
;; Bidirectional in both positive and negative contexts.
(defparameter *equal-infs*
  (list *quantifier-equals*))

;; Implicative inferences.
;; Forward in positive contexts, backward in negative contexts.
(defparameter *impl-infs*
  (list *quantifier-infs*))


;; Inference rule pairs.
;; One list of rule pairs for each positive and negative contexts.
;; The first element in the pair is the TTT pattern that matches the formula
;; and the second element is the inference rule.
(defparameter *eq-rules*
  (flatten
    (mapcar #'(lambda (inf-lst) 
                (mapcar #'(lambda (rule)
                            ;; Bidirectional.
                            (let ((le (equal-left-elem rule))
                                  (re (equal-right-elem rule)))
                              (list (list le rule)
                                    (list re (reverse-equal-rule rule)))))
                        inf-lst))
            *equal-infs*)))

(defparameter *pos-rules*
  (flatten 
    (list ;*eq-rules*
          ;; Implicative inferences to pos rules.
          (mapcar
            #'(lambda (inf-lst)
                (mapcar 
                  #'(lambda (rule)
                      (list (rule-antecedent rule) rule))
                  inf-lst))
            *impl-infs*))))

(defparameter *neg-rules*
  (flatten 
    (list ;*eq-rules*
          ;; Implicative inferences to neg rules.
          (mapcar
            #'(lambda (inf-lst)
                (mapcar 
                  #'(lambda (rule)
                      (list (rule-consequent rule) (reverse-impl-rule rule)))
                  inf-lst))
            *impl-infs*))))

