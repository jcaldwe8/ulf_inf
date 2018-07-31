
(defparameter *record-types*
  '(date-time currency us_addr))

(defparameter *ttt-noun*
   '(! lex-noun?
      lex-name-pred?
      (adj? noun?)
      (plur noun?)
      (noun? noun?)
      ; (mother-of.n |John|)
      (lex-noun? term?)
      (lex-function? term?)))

(defparameter *ttt-adj*
  '(! lex-adjective?
      (adj? adj?)
      (adv-a? adj?)))

(defparameter *ttt-adv-a*
   '(! lex-adv-a?
      (adv-a pred?)))

(defparameter *ttt-adv-e*
   '(! lex-adv-e?
      (adv-e pred?)))

(defparameter *ttt-adv-s*
   '(! lex-adv-s?
      (adv-s pred?)))

(defparameter *ttt-adv-f*
   '(! lex-adv-f?
      (adv-f pred?)))

(defparameter *ttt-pp*
   '(lex-p? term?))

(defparameter *ttt-term*
  '(! lex-pronoun?
      lex-name?
      lex-number?
      (det? noun?)
      (k noun?)
      (to verb?)
      (ka verb?)
      (ke sent?)
      ;; Record syntax.
      ($ record-type? (+ (! - term?)))
      (lex-arg-p? term?)
      ;; Rather than building a whole set of types corresponding to versions 
      ;; with the hole contained, I'll just check it dynamically.
      [*]h))

(defparameter *ttt-verb*
   '(! lex-verb?
      (verb? (? (! term? pred?)) (! term? pred?))
      (adv-a? verb?)
      (verb? (+ adv-a?))
      (aux? verb?)
      (verb? adv-a? term?)))

(defparameter *ttt-pred*
   '(! verb? noun? adj? tensed-verb? pp?
       (lex-ps? tensed-sent?)
       (lex-rel? pred?)
       (sub lex-rel? sent?)))

(defparameter *ttt-aux*
  '(! lex-aux? perf prog))

(defparameter *ttt-tensed-aux*
  '(lex-tense? aux?))

(defparameter *ttt-tensed-verb*
  '(! (lex-tense? verb?)
      (tensed-verb? (? (! term? pred?)) (! term? pred?))
      (tensed-aux? verb?)
      (adv-a? tensed-verb?)
      (tensed-verb? (+ adv-a?))
      (tensed-verb? adv-a? term?)))

(defparameter *ttt-det*
  '(! lex-det?
      (lex-detformer? adj?)))

(defparameter *ttt-sent*
  '(! (term? verb?)
      (sent? lex-coord? (+ sent?))
      (sent-mod? sent?)
      (sent? sent-mod?)
      (adv-a? term? verb?)
      (sent? sent-punct?)))

(defparameter *ttt-tensed-sent*
  '(! (term? tensed-verb?)
      (tensed-sent? lex-coord? (+ tensed-sent?))
      (sent-mod? tensed-sent?)
      (tensed-sent? sent-mod?)
      (tensed-verb? adv-a? term?)
      (tensed-sent? sent-punct?)))

(defparameter *ttt-sent-mod*
  '(!1 
      (lex-coord? (!2 tensed-sent? sent?))))

(defun noun? (x) (ttt:match-expr *ttt-noun* x))
(defun adj? (x) (ttt:match-expr *ttt-adj* x))
(defun adv-a? (x) (ttt:match-expr *ttt-adv-a* x))
(defun adv-e? (x) (ttt:match-expr *ttt-adv-e* x))
(defun adv-s? (x) (ttt:match-expr *ttt-adv-s* x))
(defun adv-f? (x) (ttt:match-expr *ttt-adv-f* x))
(defun pp? (x) (ttt:match-expr *ttt-pp* x))
(defun term? (x) (ttt:match-expr *ttt-term* x))
(defun verb? (x) (ttt:match-expr *ttt-verb* x))
(defun pred? (x) (ttt:match-expr *ttt-pred* x))
(defun det? (x) (ttt:match-expr *ttt-det* x))
(defun aux? (x) (ttt:match-expr *ttt-aux* x))
(defun tensed-aux? (x) (ttt:match-expr *ttt-tensed-aux* x))
(defun tensed-verb? (x) (ttt:match-expr *ttt-tensed-verb* x))
(defun sent? (x) (ttt:match-expr *ttt-sent* x))
(defun tensed-sent? (x) (ttt:match-expr *ttt-tensed-sent* x))
(defun sent-mod? (x) (ttt:match-expr *ttt-sent-mod* x))

(defun record-type? (x) (member x *record-types*))
(defun sent-punct? (x)
  (member x '(! ? \.)))
(defun tensed-sent-op? (x)
  (or 
    (member x '(that tht))
    (lex-ps? x)))
(defun sent-op? (x)
  (or 
    (member x '(ke))))
(defun action-reifier? (x)
  (member x '(ka to)))
(defun advformer? (x)
  (member x '(adv-a adv-e adv-s adv-f)))
(defun detformer? (x)
  (member x '(fquan nquan)))

(defparameter *type-id-fns*
  (list (list #'noun? 'noun)
        (list #'adj? 'adj)
        (list #'adv-a? 'adv-a)
        (list #'adv-e? 'adv-e)
        (list #'adv-s? 'adv-s)
        (list #'adv-f? 'adv-f)
        (list #'pp? 'pp)
        (list #'term? 'term)
        (list #'verb? 'verb)
        (list #'pred? 'pred)
        (list #'det? 'det)
        (list #'aux? 'aux)
        (list #'tensed-aux? 'tensed-aux)
        (list #'tensed-verb? 'tensed-verb)
        (list #'sent? 'sent)
        (list #'tensed-sent? 'tensed-sent)
        (list #'lex-tense? 'tense)
        (list #'sent-punct? 'sent-punct)
        (list #'sent-op? 'sent-op)
        (list #'sent-mod? 'sent-mod)
        (list #'tensed-sent-op? 'sent-op)
        (list #'action-reifier? 'action-reifier)
        (list #'advformer? 'advformer)
        (list #'detformer? 'detformer)))

(defun ulf-type? (x)
  (let ((matched (remove-if-not #'(lambda (pair) (apply (first pair) (list x)))
                                *type-id-fns*)))
    (if matched
      (mapcar #'second matched)
      '(unknown))))

(defun label-formula-types (f)
  (cond 
    ((atom f) f)
      ;(list (cons 'types (ulf-type? f)) f))
    (t (list (cons 'types (ulf-type? f)) (mapcar #'label-formula-types f)))))

