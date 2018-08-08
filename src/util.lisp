
;; Reads a file line by line and return a list of strings.
(defun read-file-lines (filename)
  (labels
    ((helper
       (in acc)
       (multiple-value-bind
         (line end?)
         (read-line in nil)
         (if end? 
           (cons line acc)
           (helper in (cons line acc))))))
    (reverse (remove-if #'null (helper (open filename) '())))))

;; Read a file by lisp objects.
;; Returns a list of lisp objects.
(defun read-file-objects (filename)
  (let ((in (open filename))
        lst)
    (when in
      (loop for entry = (read in nil)
            while entry do (setq lst (cons entry lst)))
      (close in)
      (reverse lst))))

;; Returns a slice of the list with given starting and ending indices,
;; inclusive and exclusive, respectively.
(defun slice (lst start end)
  (labels
    ((helper
       (cur index acc)
       (cond
         ;; Ran out of list.
         ((null cur) acc)
         ;; Index past end.
         ((>= index end) acc)
         ;; Recursive case, in range.
         ((and (>= index start)
               (< index end))
          (helper (cdr cur) (+ index 1) (cons (car cur) acc)))
         ;; Recursive case before start.
         (t (helper (cdr cur) (+ index 1) acc)))))
    (reverse (helper lst 0 '()))))

;; Strips aux verbs and negation from ulf once polarity is extracted
;; (remove-aux-not '((the.d man.n) ((past do.aux-s) not (know.v (that (| Mary| ((past be.v) cold.a)))))))
;; =>
;; ((the.d man.n) (know.v (that (| Mary| ((past be.v) cold.a)))))
(defun remove-aux-not (ulf)
  (remove-double-list
    (cond
      ((null ulf) nil)
      ((atom ulf) ulf)
      ((or (is-aux ulf) (equalp 'NOT ulf)) nil)
      (t (mapcar (lambda (x) (remove-aux-not x)) ulf)))))

;; Function to recursively remove doubled-up lists (e.g. (remove-double-list '(A ((B C)))) => (A (B C)))
(defun remove-double-list (tr)
  (cond
    ((null tr) nil)
    ((atom tr) tr)
    ((and (listp tr) (= 1 (length tr)) (listp (first tr))) (first tr))
    (t (mapcar (lambda (x) (remove-double-list x)) tr))))

;; Given ulf, returns segment beginning with verb, its parent, and full ulf
;; (i.e. the format required for get-segment-polarity in dynamic-polarity/dynamic-polarity.lisp)
;; Example useage:
;; (get-ulf-segments-vp '((THE.D MAN.N) ((PAST DO.AUX-S) NOT (KNOW.V (THAT (| MARY| ((PAST BE.V) COLD.A)))))))
;; =>
;; ((KNOW.V (THAT (| MARY| ((PAST BE.V) COLD.A))))
;;  ((PAST DO.AUX-S) NOT (KNOW.V (THAT (| MARY| ((PAST BE.V) COLD.A)))))
;;  ((THE.D MAN.N) ((PAST DO.AUX-S) NOT (KNOW.V (THAT (| MARY| ((PAST BE.V) COLD.A)))))))
(defun get-ulf-segments-vp (ulf)
  (let ((res (call-tfdfs-par ulf (until-vp))))
    (if res (list (first res) (third res) ulf) nil)))

;; Return true if given item is a verb (i.e. ends in .V) or tense+verb (i.e. (PAST *.V))
(defun is-verb (x)
  (cond ((and (symbolp x) (search ".V" (string x))) t)
  ((and (listp x) (= 2 (length x))
    (symbolp (first x)) (symbolp (second x)) (search ".V" (string (second x)))) t)
  (t nil)))

;; Return true if given item is a aux-s (i.e. ends in .AUX-S) or tense+aux-s (i.e. (PAST *.AUX-S))
(defun is-aux (x)
  (cond ((and (symbolp x) (search ".AUX-S" (string x))) t)
  ((and (listp x) (= 2 (length x))
    (symbolp (first x)) (symbolp (second x)) (search ".AUX-S" (string (second x)))) t)
  (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following functions define a number of recursive
;;; "thunk"-type functions for tree traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wrapper function to call tfdfs given evaluation function
;; Example useage:
;; (call-tfdfs '((A (B C)) D) (until-m 5)) => (C NIL)
;; (call-tfdfs '((A (B C)) D) (until-sym 'D)) => (D NIL)
(defun call-tfdfs (tr fn)
  (tfdfs tr (funcall fn 0)))

;; Wrapper function to call tfdfs-par given evaluation function
;; Example useage:
;; (call-tfdfs-par '((A (B C)) D) (until-m 5)) => (C NIL (B C))
;; (call-tfdfs-par '((A (B C)) D) (until-sym 'D)) => (D NIL ((A (B C)) D))
(defun call-tfdfs-par (tr fn)
  (tfdfs-par tr (funcall fn 0) nil))

;; Depth-first traversal of tree, returns element on which evaluation function 'fn' returns true
(defun tfdfs (tr fn)
    (if (null tr) (list nil fn)
      (let*
          ((res (funcall fn tr))
          (evl (first res))
          (newfn (second res)))
        (cond
          (evl (list tr nil))
          ((atom tr) (list nil newfn))
          (t
            (dolist (st tr)
              (let ((tmpres (tfdfs st newfn)))
                (if (equalp st '(A B C)) (setq testnewfn newfn))
                (if (first tmpres)
                  (return-from tfdfs tmpres))
                (setq newfn (second tmpres))))
            (list nil newfn))))))

;; Depth-first traversal of tree, returns element on which evaluation function 'fn' returns true, as
;; well as that element's parent
(defun tfdfs-par (tr fn tr-par)
    (if (null tr) (list nil fn nil)
      (let*
          ((res (funcall fn tr))
          (evl (first res))
          (newfn (second res)))
        (cond
          (evl (list tr nil tr-par))
          ((atom tr) (list nil newfn tr))
          (t
            (dolist (st tr)
              (let ((tmpres (tfdfs-par st newfn tr)))
                (if (equalp st '(A B C)) (setq testnewfn newfn))
                (if (first tmpres)
                  (return-from tfdfs-par tmpres))
                (setq newfn (second tmpres))))
            (list nil newfn tr))))))

;; Depth-first traversal of tree as in tfdfs, but uses car/cdr rather than dolist in traversal
;; (unused currently)
(defun tfdfs-car (tr fn)
  (if (null tr) (list nil fn)
    (let*
        ((res (funcall fn tr))
        (evl (first res))
        (newfn (second res)) carres)
      (cond
        (evl (list tr nil))
        ((atom tr) (list nil newfn))
        (t
          (setq carres (tfdfs-car (car tr) newfn))
          (if (first (car res)) carres
            (tfdfs-car (cdr tr) (second carres))))))))

;; Sample tfdfs evaluation function : runs until 'm' increments are reached
(defun until-m (m)
  (labels 
    ((out (n)
      (lambda (tree)
        (labels
          ((n (x)
            (list (if (= n m) t nil)
            (out (+ n 1))))
          ) ; end labels def
        (n tree))))) #'out))

;; Sample tfdfs evaluation function : runs until symbol 'sym' is reached
(defun until-sym (sym)
  (labels
    ((out (s)
      (lambda (tree)
        (labels
          ((s (x)
            (list (if (equalp x sym) t nil)
            (out s)))
          ) ; end labels def
        (s tree))))) #'out))

;; Runs until a tree having a verb as the first element is found
(defun until-vp ()
  (labels
    ((out (s)
      (lambda (tree)
        (labels
          ((s (x)
            (list (if (and (listp x) (is-verb (first x))) t nil)
            (out s)))
          ) ; end labels def
        (s tree))))) #'out))