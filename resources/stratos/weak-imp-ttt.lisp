
(DEFUN TENSE? (L) (IF (MEMBER L '(PAST PRES PERF PROG)) T NIL))

(DEFCLASS AXIOM-TTT NIL
          ((TYPE :INITARG :TYPE :INITFORM 'S)
           (POLARITY :INITARG :POLARITY :INITFORM '+)
           (RULE :INITARG :RULE)))

(DEFPARAMETER *INFERENCE-RULES*
    (LIST
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X THINK.V (THAT W))
                      (X BELIEVE.V (THAT (PROBABLY W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X PREDICT.V (THAT W))
                      (PROBABLY (X BELIEVE.V (THAT W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X PREDICT.V (THAT W))
                      (X BELIEVE.V (THAT (PROBABLY W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X SAY.V (THAT W))
                      (PROBABLY (X BELIEVE.V (THAT W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X SAY.V (THAT W))
                      (X BELIEVE.V (THAT (PROBABLY W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X SUPPOSE.V (THAT W))
                      (X BELIEVE.V (THAT (PROBABLY W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X GUESS.V (THAT W))
                      (X BELIEVE.V (THAT (PROBABLY W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X SPECULATE.V (THAT W))
                      (X BELIEVE.V (THAT (PROBABLY W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X SUSPECT.V (THAT W))
                      (X BELIEVE.V (THAT (PROBABLY W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X SURMISE.V (THAT W))
                      (X BELIEVE.V (THAT (PROBABLY W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X ADVOCATE.V (THAT W))
                      (PROBABLY (X BELIEVE.V (THAT W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X ARGUE.V (THAT W))
                      (PROBABLY (X BELIEVE.V (THAT W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X CLAIM.V (THAT W))
                      (PROBABLY (X BELIEVE.V (THAT W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X HOLD.V (THAT W))
                      (X BELIEVE.V (THAT W))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! APPEAR.V (TENSE? APPEAR.V)) (KA _!3)))
                        (PROBABLY (_!1 _!3))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! SEEM.V (TENSE? SEEM.V)) (KA _!3)))
                        (PROBABLY (_!1 _!3))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF)) (IT APPEAR.V (THAT W)) (PROBABLY W)))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF)) (IT SEEM.V (THAT W)) (PROBABLY W)))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X DOUBT.V (THAT W))
                      (NOT (X BELIEVE.V (THAT W)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((W_WFF) (X)) (X DOUBT.V (THAT W))
                      (X BELIEVE.V (THAT (PROBABLY (NOT W))))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X)) (X ATTEMPT.V (KA P))
                      (X WANT.V (KA P))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X)) (X INTEND.V (KA P))
                      (X WANT.V (KA P))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X)) (X TRY.V (KA P))
                      (X WANT.V (KA P))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X)) (X STRIVE.V (KA P))
                      (X WANT.V (KA P))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X)) (X STRUGGLE.V (KA P))
                      (X WANT.V (KA P))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X) (Y)) (X ATTACK.V Y (FOR (KA P)))
                      (X BELIEVE.V (THAT (Y P)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X) (Y)) (X BLAME.V Y (FOR (KA P)))
                      (X BELIEVE.V (THAT (Y P)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X) (Y)) (X CRITICIZE.V Y (FOR (KA P)))
                      (X BELIEVE.V (THAT (Y P)))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X)) (X ENJOY.V (KA P))
                      (X (AT-LEAST-OCCASIONALLY.ADV P))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X)) (X LIKE.V (KA P))
                      (X (AT-LEAST-OCCASIONALLY.ADV P))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(((P_PRED) (X)) (X LOVE.V (KA P))
                      (X (AT-LEAST-OCCASIONALLY.ADV P))))
     (MAKE-INSTANCE 'AXIOM-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ENABLE.V (TENSE? ENABLE.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!2 (CAN.V _!3))))))
