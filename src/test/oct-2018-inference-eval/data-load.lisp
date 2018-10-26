;;; Gene Kim 10-25-2018
;;; Code to load and parse data in the desired format.
;;; NB: these should be made more robest (e.g. to empty lines)

;;; Inference data is a hashtable from isid to a list of inference data.
;;; Each inference entry is the following list:
;;;   (isid, infid, userid, inf_text, inf_category, inf_type, temporal_reln, timestamp)
(defun load-human-inferences (filename &optional (select-isids nil))
  (labels
    ((parse-line 
       (line)
       (let ((splitline (cl-strings:split line #\Tab)))
         ;; The first three elements need to be turned into integers. 
         (append (mapcar #'cl-strings:parse-number 
                         (subseq splitline 0 3))
                 (subseq splitline 3)))))

    ;; Read in lines and place into hashtable.
    (let* ((rawlines (cdr (uiop:read-file-lines filename)))
           (parsed (mapcar #'parse-line rawlines))
           (ht (make-hash-table :test 'eql)))
      (mapcar #'(lambda (x) (let ((isid (first x)))
                              (setf (gethash isid ht)
                                    (cons x (gethash isid ht)))))
              ;; Only keep entries where the isid is a member of select-isids.
              (remove-if
                #'(lambda (x) (and select-isids
                                   (not (member (first x) select-isids))))
                parsed))
      ;; Return hash table.
      ht)))


;;; Reads a tsv into (usid, isid, sentence, ulf)
(defun load-human-ulfs (filename &optional (select-usids nil))
  (labels
    ((parse-line
       (line)
       (let ((splitline (cl-strings:split line #\Tab)))
         ;; The first two elements need to be turned into integers. 
         ;; The ULF is read into symbols (TODO: preprocess ULF names)
         (append (mapcar #'cl-strings:parse-number 
                         (subseq splitline 0 2))
                 (list (nth 2 splitline))
                 (list (read-from-string (nth 3 splitline)))))))
    ;; Only keep entries where the usid is a member of select-usids.
    (remove-if
      #'(lambda (x) (and select-usids
                         (not (member (first x) select-usids))))
      (mapcar #'parse-line 
              (cdr (uiop:read-file-lines filename))))))
  
