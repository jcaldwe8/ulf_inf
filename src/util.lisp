
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

