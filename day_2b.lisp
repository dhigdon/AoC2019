(require 'cl-ppcre)

(defun load-icode (s)
  (coerce (mapcar #'parse-integer (cl-ppcre:split "," s))
          'vector))

(defun run (icode)
  (loop for pc = 0 then (+ pc 4)
        until (= (svref icode pc) 99)
        do (case (svref icode pc)
             (1 (setf (svref icode (svref icode (+ pc 3)))
                      (+ (svref icode (svref icode (+ pc 1)))
                         (svref icode (svref icode (+ pc 2))))))
             (2 (setf (svref icode (svref icode (+ pc 3)))
                      (* (svref icode (svref icode (+ pc 1)))
                         (svref icode (svref icode (+ pc 2))))))
             (otherwise (error "Bad opcode ~A at address ~A.~%"
                               (svref icode pc)
                               pc))))
  icode)

;;; Unit tests for icode interpreter
(unless 
  (and (equalp (run (load-icode "1,0,0,0,99")) #(2 0 0 0 99))
       (equalp (run (load-icode "2,3,0,3,99")) #(2 3 0 6 99))
       (equalp (run (load-icode "2,4,4,5,99,0")) #(2 4 4 5 99 9801))
       (equalp (run (load-icode "1,1,1,4,99,5,6,0,99")) #(30 1 1 4 2 5 6 0 99)))
  (format t "Check your math"))

;;; The problem-supplied icode memory image
(defvar data "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,19,9,23,1,23,6,27,2,27,13,31,1,10,31,35,1,10,35,39,2,39,6,43,1,43,5,47,2,10,47,51,1,5,51,55,1,55,13,59,1,59,9,63,2,9,63,67,1,6,67,71,1,71,13,75,1,75,10,79,1,5,79,83,1,10,83,87,1,5,87,91,1,91,9,95,2,13,95,99,1,5,99,103,2,103,9,107,1,5,107,111,2,111,9,115,1,115,6,119,2,13,119,123,1,123,5,127,1,127,9,131,1,131,10,135,1,13,135,139,2,9,139,143,1,5,143,147,1,13,147,151,1,151,2,155,1,10,155,0,99,2,14,0,0")

;;; Iterate through all opcodes to try to find an input that generates the given output 
(defun run-test (noun verb)
  (declare (fixnum noun verb))
  (let ((code (load-icode data)))
    (setf (svref code 1) noun)
    (setf (svref code 2) verb)
    (run code)
    (svref code 0)))

;;; part a's test
(defun test-a ()
  (format t "Position 0 = ~A~%" (run-test 12 2)))

;;; part b's test
(defun test-b (ans)
  (dotimes (noun 100)
    (dotimes (verb 100)
      (when (= ans (run-test noun verb))
        (format t "Parameters = Noun ~A  Verb ~A~%" noun verb)
        (return-from test-b (values noun verb))))))

