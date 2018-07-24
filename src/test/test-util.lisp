
(defun test-print (fn-name in &key (depolarize nil))
  (let (out)
    (format t "====TEST-PRINT====~%")
    (format t "Function:~%~s~%" fn-name)
    (format t "Input:~%~s~%" in)
    (setq out (funcall fn-name in))
    (format t "Output:~%~s~%" out)
    (if depolarize
      (progn (format t "Depolarized Input: ~s~%" (depolarize in))
             (format t "Depolarized Output: ~s~%" (depolarize out))))
    (format t "~%~%")))

(defun run-test-prints (fn-name inputs &key (depolarize nil))
  (mapcar #'(lambda (x) (test-print #'normalize-ulf x :depolarize depolarize))
          inputs))



