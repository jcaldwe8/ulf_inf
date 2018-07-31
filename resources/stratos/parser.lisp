;;; Ben Kane 7-17-2018
;;;
;;; Parser tool used to read in .lisp file containing implicative inference rules in Epilog form and
;;; convert them to TTT rules. Rules are stored as instances of "implicative-rule-ttt" class, with each
;;; instance containing the type ('S or 'S-PRS (presuppositional)) and polarity of each rule.

(load "../../ttt/src/load")

;; Read in imp axiom code line-by-line, ignoring empty lines and comments, and return a list of strings for each line of code
(defun read-code-lines (filename)
  (with-open-file (stream filename)
    (append (remove nil (loop for line = (read-line stream nil)
          while line
          collect (if (< (length line) 1) nil (if (equalp (subseq line 0 1) ";") nil line)))) '("end"))))

;; Combine multi-line code and return each axiom as a list structure
(defun parse-code-as-list (filename)
  (mapcar #'read-from-string (remove nil
    (let ((cur-chunk nil))
      (mapcar (lambda (line) (cond
        ((or (equalp (subseq line 0 3) "(s ") (and (> (length line) 8) (equalp (subseq line 0 8) "(store-p")))
          (let ((ret cur-chunk)) (setf cur-chunk (remove-indent line)) ret))
        ((= (length (remove-indent (subseq line 0 3))) 0)
          (progn (setf cur-chunk (concatenate 'string cur-chunk " " (remove-indent line))) nil))
        ((equalp line "end") cur-chunk) (t nil))) (read-code-lines filename))))))

;; Converts "store-prs-ir" flattened format to "s" nested format
(defun normalize-format-nested (axioms)
  (ttt:apply-rule
    '(/
      (store-prs-ir '(((_!1) (? (_!2)) (? (_!3))) _!4 _!5))
      (s-prs '((add-type! _!1) (fix-var! _!1) (constr-nested! _!2 _!3 _!4 _!5))))
  axioms))

;; Converts "s" nested format to "store-prs-ir" flattened format
(defun normalize-format-flattened (axioms)
  (ttt:apply-rules
    '((/ (s (!
      '(is-type? _!1 (all _!2 (all _!3 (_!4 => _!5))))
      '(is-type? _!1 (all _!2 (_!4 => _!5)))
      '(is-type? _!1 (_!4 => _!5))
      '(all _!2 (all _!3 (_!4 => _!5)))
      '(all _!2 (_!4 => _!5))))
    ('s '((constr-flattened! _!1 _!2 _!3) _!4 _!5)))
    (/ store-prs-ir 's-prs))
  axioms))

;; Adds polarity as explicit element in rule
(defun add-polarity (axioms)
  (ttt:apply-rules '(
    (/ ((! 's 's-prs) '(_!1 (^* not) _!2)) (! '- '(_!1 ^* _!2)))
    (/ ((! 's 's-prs) '(_!1 _!2 _!3)) (! '+ '(_!1 _!2 _!3)))
  ) axioms))

;; Converts axioms to TTT rules
(defun map-to-ttt (axioms)
  (ttt:apply-rules
    '((/ ;; ((x VERB.V (that w)) => w)
      '(_! (!1 (x verb? (that w)) (not (x verb? (that w)))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (x ((! verb? (tense? verb?)) (that w))) !2)))
    (/ ;; ((it VERB.V (that w)) => (probably w))
      '(_! (!1 (it verb? (that w)) (not (it verb? (that w)))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (it ((! verb? (tense? verb?)) (that w))) !2)))
    (/ ;; ((x (pasv VERB.V) (that w)) => w)
      '(_! (!1 (x (pasv verb?) (that w)) (not (x (pasv verb?) (that w)))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (x ((pasv verb?) (that w))) !2)))
    (/ ;; (((that w) VERB.V x) => w)
      '(_! (!1 ((that w) verb? x) (not ((that w) verb? x))) (!2 pos-neg? (modf? pos-neg?)))
       (map-vars! '(/ ((that w) ((! verb? (tense? verb?)) x)) !2)))
    (/ ;; (((that w) (pasv VERB.V)) => w)
      '(_! (!1 ((that w) (pasv verb?)) (not ((that w) (pasv verb?)))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ ((that w) (pasv verb?)) !2)))
    (/ ;; ((x VERB.V y (that w)) => w)
      '(_! (!1 (x verb? y (that w)) (not (x verb? y (that w)))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (x ((! verb? (tense? verb?)) (! (+ y (that w)) (y (that w))))) !2)))
    (/ ;; ((x VERB.V (ka p)) => (x p))
      '(_! (!1 (x verb? (ka p)) (not (x verb? (ka p)))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (x ((! verb? (tense? verb?)) (ka p))) !2)))
    (/ ;; ((x (pasv VERB.V) (ka p)) => (x p))
      '(_! (!1 (x (pasv verb?) (ka p)) (not (x (pasv verb?) (ka p)))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (x ((pasv verb?) (ka p))) !2)))
    (/ ;; ((past (not (x VERB.V (ka p)))) => (x p))
      '(_! (past (not (x verb? (ka p)))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (x ((past verb?) (ka p))) !2)))
    (/ ;; ((x VERB.V (ka p) y) => (y p))
      '(_! (!1 (x verb? (ka p) y) (not (x verb? (ka p) y))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (x ((! verb? (tense? verb?)) (! (+ y (ka p)) (y (ka p))))) !2)))
    (/ ;; ((x VERB.V y (ka p)) => (x p))
      '(_! (!1 (x verb? y (ka p)) (not (x verb? y (ka p)))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (x ((! verb? (tense? verb?)) (! (+ y (ka p)) (y (ka p))))) !2)))
    (/ ;; ((x VERB.V y (for (ka p))) => (x believe.v (that (y p))))
      '(_! (!1 (x verb? y (for (ka p))) (not (x verb? y (for (ka p))))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (x ((! verb? (tense? verb?)) (! (+ y (for (ka p))) (y (for (ka p)))))) !2)))
    (/ ;; ((x VERB.V y) => (x is-a y))
      '(_! (!1 (x verb? y) (not (x verb? y))) (!2 pos-neg? (modf? pos-neg?)))
      (map-vars! '(/ (x ((! verb? (tense? verb?)) y)) !2))))
    (add-polarity (normalize-format-flattened axioms))))

;; Converts TTT rules to class instances
(defun to-classes (rules)
  (append
    '((defun tense? (l) (if (member l '(PAST PRES PERF PROG)) t nil))
    (defclass implicative-rule-ttt ()
      ((type \:initarg \:type \:initform 'S)
      (polarity \:initarg \:polarity \:initform '+)
      (rule \:initarg \:rule))))  
    (list (list 'defparameter '*infer-from-implicatives-rules*
      (append '(list) (mapcar (lambda (x)
        (list 'make-instance ''implicative-rule-ttt '\:type (first x) '\:polarity (second x) '\:rule (third x)))
      rules))))))

;; Remove indent in string of code
(defun remove-indent (string)
  (string-trim 
    '(#\Space #\Newline #\Backspace #\Tab 
      #\Linefeed #\Page #\Return #\Rubout) string))

;; Checks if symbol is a valid type specification
(defun is-type? (symbol)
  (cond ((not (symbolp symbol)) nil) ((member symbol '(all_wff all_pred)) t) (t nil)))

;; Adds proper type/quantifier symbol for a variable
(defun add-type! (var)
  (cond
    ((equalp var 'w_wff) 'all_wff)
    ((equalp var 'p_pred) 'all_pred)
    ((or (equalp var 'x) (equalp var 'y)) 'all)))

;; Converts wff and pred variables between two rule formats
(defun fix-var! (var)
  (cond
    ((equalp var 'w_wff) 'w) ((equalp var 'p_pred) 'p)
    ((equalp var 'w) 'w_wff) ((equalp var 'p) 'p_pred)
    (t var)))

;; Constructs nested representation depending on which variables are present
(defun constr-nested! (x y p q)
  (let ((f (list p '=> q)))
    (cond
      ((and (equalp x 'x) (equalp y 'y))
        (list 'all 'x (list 'all 'y f)))
      ((equalp x 'x)
        (list 'all 'x f))
      (t f))))

;; Constructs flattened representation depending on which variables are present
(defun constr-flattened! (var x y)
  (append (if (or (equalp var 'w) (equalp var 'p)) (list (list (fix-var! var))) nil)
    (append (if (equalp x 'x) (list (list x)) nil)
      (append (if (equalp y 'y) (list (list y)) nil) '()))))

;; Checks if symbol is verb (i.e. ____.v)
(defun verb? (symbol)
  (cond ((and (symbolp symbol) (search ".V" (string symbol))) t)
  (t nil)))

;; Checks if symbol is particular modifier (allowed: 'past' and 'probably')
(defun modf? (symbol)
  (if (member symbol '(PAST PROBABLY)) t nil))

;; Checks if symbol is positive (e.g. 'w') or negative (e.g. '(not w)')
(defun pos-neg? (symbol)
  (let ((allowed '(w (x p) (y p) (y (can.v p)) (x is-a y) (x believe.v (that w))
    (x believe.v (that (probably w))) (x believe.v (that (probably (not w))))
    (x believe.v (that (y p))) (x (at-least-occasionally.adv p)) (x want.v (ka p)))))
  (if (or
    (and (listp symbol) (= (length symbol) 2) (equalp (first symbol) 'not) (member (second symbol) allowed :test 'equal))
    (member symbol allowed :test 'equal)) t nil)))

;; Maps each axiom variable to a corresponding TTT wildcard
;; x => _!1, y => _!2, w,p => _!3
(defun map-vars! (axiom)
  (ttt:apply-rules
    '((/ x _!1) (/ y _!2) (/ (! w p) _!3))
    axiom))





;; 
;; Execute parser
;; 
(with-open-file (stream "weak-imp-ttt.lisp" :direction :output :if-exists :supersede)
   (mapc (lambda (x) (format stream "~%~a~%" x)) (to-classes (map-to-ttt (parse-code-as-list "weak-imp.lisp")))))

;; (load "parser.lisp")