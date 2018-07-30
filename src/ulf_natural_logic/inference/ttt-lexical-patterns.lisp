;; ULF lexical patterns identified in TTT.

(defparameter *tense* '(past pres))
(defparameter *detformer* '(nquan fquan))
(defparameter *suffixless-coordinator* '(and or but because))
(defparameter *suffixless-lexical-determiners*
  '(some a an every each many few all no both neither the))

;; Check if *x* has the *suffix* extension. 
(defun suffix-check (x suffix)
  (match-re (concatenate 'string "^\(\\w\|\\d\|-\)\+\\." suffix "$") 
            (format nil "~s" x)))

(defun lex-noun? (x)
  (suffix-check x "N"))

(defun lex-function? (x)
  (suffix-check x "F"))

(defun lex-pronoun? (x)
  (suffix-check x "PRO")) 

(defun lex-verb? (x)
  (suffix-check x "V"))

(defun lex-adjective? (x)
  (suffix-check x "A"))

(defun lex-p? (x)
  (suffix-check x "P"))

(defun lex-arg-p? (x)
  (match-re (concatenate 'string "^\(\\w\|\\d\|-\)\+\\-ARG.P$") 
            (format nil "~s" x)))

(defun lex-ps? (x)
  (suffix-check x "PS"))

(defun lex-prep? (x)
  (or (lex-p? x)
      (lex-ps? x)))

(defun lex-rel? (x)
  (suffix-check x "REL"))

(defun lex-adverb? (x)
  (suffix-check x "ADV"))


(defun lex-det? (x)
  (or (suffix-check x "D")
      (member x *suffixless-lexical-determiners*)))

(defun lex-coord? (x)
  (or
    (member x *suffixless-coordinator*)
    (suffix-check x "CC")))

; Auxiliaries.
(defun lex-aux-s? (x)
  (suffix-check x "AUX-S"))
(defun lex-aux-v? (x)
  (suffix-check x "AUX-V"))
(defun lex-aux? (x)
  (or 
    (lex-aux-s? x)
    (lex-aux-v? x)))

(defun lex-number? (x)
  (numberp x))

;; Matches a name predicate.
(defun lex-name-pred? (x)
  (match-re "^\\|\[\^\\|\]\+\\.N\\|$" (format nil "~s" x)))

;; Matches a regular name.
(defun lex-name? (x)
  (and
    (match-re "^\\|\[\^\\|\]\+\\|$" (format nil "~s" x))
    (not (lex-name-pred? x))))

; Adverbs
(defun lex-adv-a? (x)
  (suffix-check x "ADV-A"))
(defun lex-adv-s? (x)
  (suffix-check x "ADV-S"))
(defun lex-adv-e? (x)
  (suffix-check x "ADV-E"))
(defun lex-adv-f? (x)
  (suffix-check x "ADV-F"))
(defun lex-adv-formula? (x)
  (or
    (lex-adv-s? x)
    (lex-adv-e? x)
    (lex-adv-f? x)))
(defun lex-adv? (x)
  (or 
    (lex-adv-a? x)
    (lex-adv-s? x)
    (lex-adv-e? x)
    (lex-adv-f? x)))

(defun lex-tense? (x)
  (member x *tense*))

(defun lex-detformer? (x)
  (member x *detformer*))

