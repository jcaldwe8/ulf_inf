;;; Ben Kane 7-22-2018
;;;
;;; Parsed into TTT rules from Karl Stratos rules from Epilog package using /resources/stratos/parser.lisp.

;; TTT computable predicate; accepts tense/aspect operators in front of verb
;; NOTE: Can be restricted to just "past" and "pres"
(DEFUN TENSE? (L) (IF (MEMBER L '(PAST PRES PERF PROG)) T NIL))

;; Define class for implicative rules containing type ('S or 'S-PRS), polarity ('+ or '-) and the rule
(DEFCLASS IMPLICATIVE-RULE-TTT NIL
          ((TYPE :INITARG :TYPE :INITFORM 'S)
           (POLARITY :INITARG :POLARITY :INITFORM '+)
           (RULE :INITARG :RULE)))

(DEFPARAMETER *INFER-FROM-IMPLICATIVE-RULES-WEAK*
;``````````````````````````````````````````````````````````
; For implicative rules, e.g. "John knows that Mary went to
; the game" => "Mary went to the game".
;
; Each rule is stored as a class. The following values can be accessed using 'slot-value':
; :TYPE      :  Whether or not rule is presuppositional ('S-PRS vs. 'S)
; :POLARITY  :  Whether a rule applies to a positive ('+) or negative ('-) polarity
; :RULE      :  The specific TTT rule for a particular implicative verb
;
    (LIST
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! THINK.V (TENSE? THINK.V)) (THAT _!3)))
                        (_!1 BELIEVE.V (THAT (PROBABLY _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! PREDICT.V (TENSE? PREDICT.V)) (THAT _!3)))
                        (PROBABLY (_!1 BELIEVE.V (THAT _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! PREDICT.V (TENSE? PREDICT.V)) (THAT _!3)))
                        (_!1 BELIEVE.V (THAT (PROBABLY _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! SAY.V (TENSE? SAY.V)) (THAT _!3)))
                        (PROBABLY (_!1 BELIEVE.V (THAT _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! SAY.V (TENSE? SAY.V)) (THAT _!3)))
                        (_!1 BELIEVE.V (THAT (PROBABLY _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! SUPPOSE.V (TENSE? SUPPOSE.V)) (THAT _!3)))
                        (_!1 BELIEVE.V (THAT (PROBABLY _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! GUESS.V (TENSE? GUESS.V)) (THAT _!3)))
                        (_!1 BELIEVE.V (THAT (PROBABLY _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! SPECULATE.V (TENSE? SPECULATE.V))
                          (THAT _!3)))
                        (_!1 BELIEVE.V (THAT (PROBABLY _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! SUSPECT.V (TENSE? SUSPECT.V)) (THAT _!3)))
                        (_!1 BELIEVE.V (THAT (PROBABLY _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! SURMISE.V (TENSE? SURMISE.V)) (THAT _!3)))
                        (_!1 BELIEVE.V (THAT (PROBABLY _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ADVOCATE.V (TENSE? ADVOCATE.V))
                          (THAT _!3)))
                        (PROBABLY (_!1 BELIEVE.V (THAT _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! ARGUE.V (TENSE? ARGUE.V)) (THAT _!3)))
                        (PROBABLY (_!1 BELIEVE.V (THAT _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! CLAIM.V (TENSE? CLAIM.V)) (THAT _!3)))
                        (PROBABLY (_!1 BELIEVE.V (THAT _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! HOLD.V (TENSE? HOLD.V)) (THAT _!3)))
                        (_!1 BELIEVE.V (THAT _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! APPEAR.V (TENSE? APPEAR.V)) (KA _!3)))
                        (PROBABLY (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! SEEM.V (TENSE? SEEM.V)) (KA _!3)))
                        (PROBABLY (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (IT
                         ((! APPEAR.V (TENSE? APPEAR.V)) (THAT _!3)))
                        (PROBABLY _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (IT ((! SEEM.V (TENSE? SEEM.V)) (THAT _!3)))
                        (PROBABLY _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! DOUBT.V (TENSE? DOUBT.V)) (THAT _!3)))
                        (NOT (_!1 BELIEVE.V (THAT _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! DOUBT.V (TENSE? DOUBT.V)) (THAT _!3)))
                        (_!1 BELIEVE.V (THAT (PROBABLY (NOT _!3))))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ATTEMPT.V (TENSE? ATTEMPT.V)) (KA _!3)))
                        (_!1 WANT.V (KA _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! INTEND.V (TENSE? INTEND.V)) (KA _!3)))
                        (_!1 WANT.V (KA _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! TRY.V (TENSE? TRY.V)) (KA _!3)))
                        (_!1 WANT.V (KA _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! STRIVE.V (TENSE? STRIVE.V)) (KA _!3)))
                        (_!1 WANT.V (KA _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! STRUGGLE.V (TENSE? STRUGGLE.V)) (KA _!3)))
                        (_!1 WANT.V (KA _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ATTACK.V (TENSE? ATTACK.V))
                          (! (+ _!2 (FOR (KA _!3)))
                           (_!2 (FOR (KA _!3))))))
                        (_!1 BELIEVE.V (THAT (_!2 _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! BLAME.V (TENSE? BLAME.V))
                          (! (+ _!2 (FOR (KA _!3)))
                           (_!2 (FOR (KA _!3))))))
                        (_!1 BELIEVE.V (THAT (_!2 _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! CRITICIZE.V (TENSE? CRITICIZE.V))
                          (! (+ _!2 (FOR (KA _!3)))
                           (_!2 (FOR (KA _!3))))))
                        (_!1 BELIEVE.V (THAT (_!2 _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! ENJOY.V (TENSE? ENJOY.V)) (KA _!3)))
                        (_!1 (AT-LEAST-OCCASIONALLY.ADV _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! LIKE.V (TENSE? LIKE.V)) (KA _!3)))
                        (_!1 (AT-LEAST-OCCASIONALLY.ADV _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! LOVE.V (TENSE? LOVE.V)) (KA _!3)))
                        (_!1 (AT-LEAST-OCCASIONALLY.ADV _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ENABLE.V (TENSE? ENABLE.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!2 (CAN.V _!3))))))
