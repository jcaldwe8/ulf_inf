;;; Gene Kim 10-19-2018
;;; Runs top level inference evaluation for the experiments in October 2018 on
;;; implicatives, counterfactuals, requests, and questions.

;; TODO: wrap this all in function like done for axiomatizing-wordnet-verbs (function runtest).

;; This probably should go in a configuration file.
(defparameter *human-inference-file* "test/oct-2018-inference-eval/isid-inf-out.tsv")
(defparameter *human-ulf-file* "test/oct-2018-inference-eval/usid-isid-sent-ulf-out.tsv")

;; This file restricts the experiment to a select set of usids.
(defparameter *selected-usid-file* "test/oct-2018-inference-eval/dev-usids.txt")




;;; 1. Load selected usids.
(setq selected-usids (mapcar #'cl-strings:parse-number
                             (uiop:read-file-lines *selected-usid-file*)))

;;; 2. Load human ULF annotations.
;;; Assume list of (usid, isid, sentence, ulf) 
(setq human-ulfs (load-human-ulfs *human-ulf-file* selected-usids))
;;; Get isids corresponding to selected usids.
(setq selected-isids
      (remove-if 
        #'null 
        (mapcar #'(lambda (usid)
                    (reduce #'(lambda (acc new)
                                (cond (acc acc) 
                                      ((eql usid (first new)) (second new))
                                      (t nil)))
                            human-ulfs :initial-value nil))
                selected-usids)))
                            

;;; 3. Load human inferences.
;;; Inference data is a hashtable from isid to a list of inference data.
;;; Each inference entry is the following list:
;;;   (isid, infid, userid, inf_text, inf_category, inf_type, temporal_reln, timestamp)
(setq human-infs (load-human-inferences *human-inference-file* selected-isids))


;;; 3. Run inferences.
;;; TODO: add infer-all-parameters as optional parameters to this function..
;;; Gets the inferences of the given ULF and inference parameters.
(defun get-ulf-infs (ulf &optional inf-params)
  (mapcar #'result-formula (infer-all ulf)))

;;; auto-infs:
;;;   Each human-ulf info is paired with the list of inferences for that ulf.
(setq auto-infs 
      (mapcar #'(lambda (x)
                  (let ((ulf (fourth x)))
                    (list x (get-ulf-infs ulf))))
              human-ulfs))

;;; 4. Evaluate inferences.
;;; eval-single-sent is a curried function that takes the human inference 
;;; dataset and returns a function that evaluates an automated inference
;;; over that human inference set.



;; Basic inference matching function that calls ulf2string on the generated
;; inference and checks for exact match with the human inference.
(defun ulf2string-match (ginf hinf)
  (let ((gstr (ulf-to-string ginf))
        (did-match (equal gstr hinf)))
    (if did-match
      (list t (list ginf gstr hinf))
      (list nil (list ginf gstr hinf)))))

;; Matches a list of generated inferences against human inferences.
;; Returns a list of (hinf, gen-inf, whether it matched, match-metadata)
;; matchfn must take gen-inf and hinf and return (whether it matched, metadata)
(defun match-gen-human-infs (gen-infs hinfs matchfn)
  (labels
    ((match-to-single-human-inf
       (ginfs hinf)
       (cond
         ((null ginfs) (list hinf nil nil nil))
         (t (let* ((ginf (car ginfs))
                   (matchres (funcall matchfn ginf hinf))
                   (did-match (first matchres))
                   (match-metadata (second matchres)))
              (if did-match
                ;; Matched, so return relevant info.
                (list hinf gen-inf did-match match-metadata)
                ;; No match, recurse.
                (match-to-single-human-inf (cdr ginfs) hinf))))))
     ) ; end of labels defs.

  ;; Body of main function.
  (cond
    ;; No more human inferences to handle so return nil.
    ((null hinfs) nil)
    (t
      (let ((matchres (match-to-single-human-inf gen-infs (car hinfs)))
            (recres (match-gen-human-infs gen-infs (cdr hinfs) matchfn)))
        ;; Add match result to recursive case.
        (cons matchres recres))))))


;;; A curried function that takes a hash table of human-infs (keyed by isid),
;;; and generated inferences, and then returns the result data.
;;; Return data format:
;;;   (ulfinfo, generated inf info, human infs for isid, matched info, nomatched info)
(defun eval-single-sent (human-infs)
  ;; TODO: maybe add human-infs preprocessing code.
    (lambda (gen-inf-info)
      (let* (
             ;; Break down ulf info.
             (ulfinfo (first gen-inf-info))
             (usid (first ulfinfo))
             (isid (second ulfinfo))
             (sent (third ulfinfo))
             (ulf (fourth ulfinfo))
             ;; Break down generated inferences.
             (gen-infs (second gen-inf-info))
             ;; Get appropriate human inferences.
             (hinfs (gethash isid human-infs))
             matchdata matched nomatched)

        ;; For each human inference, try to match with a generated inference.  
        (setq matchdata (match-gen-human-infs gen-infs hinfs #'ulf2string-match))
        
        ;; Compute coverage.
        (setq nomatched (remove-if #'(lambda (x) (not (null (second x)))) matchdata))
        (setq matched (remove-if-not #'(lambda (x) (not (null (second x)))) matchdata))

        (format t "~%==Info for USID: ~s==~%" usid)
        (format t "~s out of ~s human inferences matched for usid: ~s~%" (length matched) (length matchdata) usid) 
        (format t "-- match details --~%")
        (format t "-- MATCHED --~%")
        (mapcar #'(lambda (x) (format t "~s~%" x)) matched)
        (format t "~%-- NOT MATCHED --~%")
        (mapcar #'(lambda (x) (format t "~s~%" x)) nomatched)
        (format t "~%-- GENERATED --~%")
        (mapcar #'(lambda (x) (format t "~s~%" x)) gen-infs)

        ;; For each non-sitational inf_category in human inferences, determine 
        ;; in how many categories generation agrees on whether an inference 
        ;; exists.
        ;; TODO

        ;; Return all the info.
        (list ulfinfo gen-infs hinfs matched nomatched))))


;; TODO: add a configurable eval selector if we end up having multiple
;;    OR just run them all if they don't take too long.
(setq ind-ulf-evals (mapcar (eval-single-sent human-infs) auto-infs))

(format t "~s sentences" (length ind-ulf-evals))

; TODO
;(setq collected-eval (combine-evals ind-evals))


;;; 5. Output eval results (to stdout and to files)
; TODO
;(write-eval-results ind-evals collected-eval)

(print "Done!")

