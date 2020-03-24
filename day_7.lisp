(load "intcode")

(defparameter *thruster-program*
  (parse-intcode "3,8,1001,8,10,8,105,1,0,0,21,42,55,64,77,94,175,256,337,418,99999,3,9,102,4,9,9,1001,9,5,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,101,5,9,9,4,9,99,3,9,1002,9,4,9,4,9,99,3,9,102,4,9,9,101,5,9,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,1002,9,5,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99"))


(defun run-test (program pa pb pc pd pe)
  (let ((a (make-interp :memory (copy-seq program)))
        (b (make-interp :memory (copy-seq program)))
        (c (make-interp :memory (copy-seq program)))
        (d (make-interp :memory (copy-seq program)))
        (e (make-interp :memory (copy-seq program))))
    (interp-add-input a pa)
    (interp-add-input b pb)
    (interp-add-input c pc)
    (interp-add-input d pd)
    (interp-add-input e pe)
    (interp-add-input a 0)
    (interp-eval a)
    (interp-add-input b (interp-read-output a))
    (interp-eval b)
    (interp-add-input c (interp-read-output b))
    (interp-eval c)
    (interp-add-input d (interp-read-output c))
    (interp-eval d)
    (interp-add-input e (interp-read-output d))
    (interp-eval e)
    (interp-read-output e)))

(defun run-test2 (program pa pb pc pd pe)
  "All units with outputs directed to inputs.
  Run until all units halt, return final output of last unit"
  (let ((a (make-interp :memory (copy-seq program)))
        (b (make-interp :memory (copy-seq program)))
        (c (make-interp :memory (copy-seq program)))
        (d (make-interp :memory (copy-seq program)))
        (e (make-interp :memory (copy-seq program))))

    ;; first, link up the inputs and outputs
    (setf (interp-input b) (interp-output a))
    (setf (interp-input c) (interp-output b))
    (setf (interp-input d) (interp-output c))
    (setf (interp-input e) (interp-output d))
    (setf (interp-input a) (interp-output e))

    ;; Now, jam the first input, which is the phase
    (interp-add-input a pa)
    (interp-add-input b pb)
    (interp-add-input c pc)
    (interp-add-input d pd)
    (interp-add-input e pe)

    ;; finally, start the input off with a 0
    (interp-add-input a 0)

    ;; Process all thrusters until their programs terminate
    (loop (interp-eval a)
          (interp-eval b)
          (interp-eval c)
          (interp-eval d)
          (interp-eval e)
          ;; We actually only have to process until 'e' exits,
          ;; because all thrusters run the same number of cycles,
          ;; and if any one of them halts early, the others will starve
          ;; for inputs.
          (when (eq 'exit (interp-halted e)) (return))
          )
    ;; A will not have read this value, since the process should have
    ;; terminated
    (interp-read a)))


(defun day7a ()
  (let ((best 0)
        (settings nil))
    (dolist (try (hadt:permute '(0 1 2 3 4)) (values settings best))
      (let ((v (apply #'run-test *thruster-program* try)))
        (when (> v best)
          (format t "Better ~A ~A~%" v try)
          (setq best v
                settings try))))))



(defun day7b ()
  (let ((best 0)
        (settings nil))
    (dolist (try (hadt:permute '(5 6 7 8 9)) (values settings best))
      (let ((v (apply #'run-test2 *thruster-program* try)))
        (when (> v best)
          (format t "Better ~A ~A~%" v try)
          (setq best v
                settings try))))))



;;;;;;
;(defparameter *test1* (parse-intcode "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
;(defparameter *test2* (parse-intcode "3,23,3,24,1002,24,10,24,1002,23,-1,23,
;                                     101,5,23,23,1,24,23,23,4,23,99,0,0"))
;(defparameter *test3* (parse-intcode "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
;                                     1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))
;
;(format t "Run test 1 = ~A, should be 43210~%" (run-test *test1* 4 3 2 1 0))
;(format t "Run test 2 = ~A, should be 54321~%" (run-test *test2* 0 1 2 3 4))
;(format t "Run test 3 = ~A, should be 65210~%" (run-test *test3* 1 0 4 3 2))

;(defparameter *test1* (parse-intcode "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))

;(defparameter *test2* (parse-intcode "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))

;(setf *trace-intcode* t)
;(trace interp-halted interp-eval)
;(format t "Run test 1 = ~A, should be 139629729~%" (run-test2 *test1* 9 8 7 6 5))
;(format t "Run test 2 = ~A, should be 18216~%" (run-test2 *test2* 9 7 8 5 6))

