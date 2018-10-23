;;; Gene Kim 10-19-2018
;;; Runs top level inference evaluation for the experiments in October 2018 on
;;; implicatives, counterfactuals, requests, and questions.

;; TODO: wrap this all in function like done for axiomatizing-wordnet-verbs (function runtest).

;; This probably should go in a configuration file.
(defparameter *human-inference-file* "TODO")
(defparameter *human-ulf-file* "TODO")

;;; 1. Load human inferences.
(setq human-infs (load-human-inferences *human-inference-file*))

;;; 2. Load human ULF annotations.
;;; Assume list of (usid, isid, sentence, ulf) 
(setq human-ulfs (load-human-ulfs *human-ulf-file*))


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
;; TODO: add a configurable eval selector if we end up having multiple
;;    OR just run them all if they don't take too long.
(setq ind-evals (mapcar (eval-single-sent human-infs) auto-infs))
(setq collected-eval (combine-evals ind-evals))


;;; 5. Output eval results (to stdout and to files)
(write-eval-results ind-evals collected-eval)

(print "Done!")

