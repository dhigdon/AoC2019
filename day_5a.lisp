(require 'cl-ppcre)

(defun parse-fixnum (str)
  (the fixnum (parse-integer (string-trim " " str))))

(defun parse-intcode (s)
  (coerce (mapcar #'parse-fixnum (cl-ppcre:split "," s))
          'vector))

;; Intcode CPU

;;; Addressing mode
(deftype adr-mode ()
  `(ftype (function (simple-vector fixnum) fixnum)))

(declaim (adr-mode adr-mode-pos adr-mode-imm))
(defun adr-mode-pos (mem val) (svref mem val)) 
(defun adr-mode-imm (mem val) (progn mem val))

(defun decode-adr-mode (code len)
  "Given an opcode, decodes the addressing modes into a list
  of functions to call on the parameters.
  Returns 'len' functions"
  (let ((result '())
        (modes (floor code 100))
        (mode 0))
    (declare (fixnum modes mode))
    (dotimes (i len (reverse result))
      (multiple-value-setq (modes mode) (floor modes 10))
      (case mode
        (0 (setf result (cons #'adr-mode-pos result)))
        (1 (setf result (cons #'adr-mode-imm result)))))))

(defun op-param (mem adr mode)
  "Applies 'mode' to 'mem' at 'adr' to get the opcode parameter value"
  (declare (simple-vector mem)
           (fixnum adr)
           )
  (funcall mode mem (svref mem adr)))

(defparameter *trace-intcode* nil)

(defun run-intcode (icode)
  (do ((pc 0 pc))
    ((>= pc (length icode))
     (error "No END opcode found"))
    (let ((opcode (svref icode pc)))
      (case (mod opcode 100)
        (1  ;; ADD
         (let* ((mode (decode-adr-mode opcode 2))
                (op1 (op-param icode (+ pc 1) (car mode)))
                (op2 (op-param icode (+ pc 2) (cadr mode)))
                (dest (svref icode (+ pc 3))))
           (when *trace-intcode*
             (format t "~4A ADD(~A) ~A ~A ~A~%" pc opcode op1 op2 dest))
           (setf (svref icode dest) (+ op1 op2)))
         (incf pc 4))

        (2  ;; MUL
         (let* ((mode (decode-adr-mode opcode 2))
                (op1 (op-param icode (+ pc 1) (car mode)))
                (op2 (op-param icode (+ pc 2) (cadr mode)))
                (dest (svref icode (+ pc 3))))
           (when *trace-intcode*
             (format t "~4A MUL(~A) ~A ~A ~A~%" pc opcode op1 op2 dest))
           (setf (svref icode dest) (* op1 op2)))
         (incf pc 4))

        (3  ;; READ
         (when *trace-intcode*
           (format t "~4A READ(~A) ~A~%" pc opcode (svref icode (+ pc 1))))
         (setf (svref icode (svref icode (+ pc 1))) (parse-fixnum (read-line)))
         (incf pc 2))

        (4  ;; WRITE
         (let* ((mode (decode-adr-mode opcode 1))
                (op1 (op-param icode (+ pc 1) (car mode))))
           (when *trace-intcode*
             (format t "~4A WRITE(~A) ~A~%" pc opcode op1))
           (format t "~A~%" op1))
         (incf pc 2))

        (5  ;; Jump-if-true
         (let* ((mode (decode-adr-mode opcode 2))
                (test (op-param icode (+ pc 1) (car mode)))
                (dest (op-param icode (+ pc 2) (cadr mode))))
           (when *trace-intcode*
             (format t "~4A JNZ(~A) ~A ~A~%" pc opcode test dest))
           (if (zerop test)
             (incf pc 3)
             (setf pc dest))))

        (6  ;; Jump-if-false
         (let* ((mode (decode-adr-mode opcode 2))
                (test (op-param icode (+ pc 1) (car mode)))
                (dest (op-param icode (+ pc 2) (cadr mode))))
           (when *trace-intcode*
             (format t "~4A JZ ~A ~A ~A~%" pc opcode test dest))
           (if (zerop test)
             (setf pc dest)
             (incf pc 3))))

        (7  ;; Less-than
         (let* ((mode (decode-adr-mode opcode 2))
                (op1 (op-param icode (+ pc 1) (car mode)))
                (op2 (op-param icode (+ pc 2) (cadr mode)))
                (dest (svref icode (+ pc 3))))
           (when *trace-intcode*
             (format t "~4A LT(~A) ~A ~A ~A~%" pc opcode op1 op2 dest))
           (setf (svref icode dest) (if (< op1 op2) 1 0)))
         (incf pc 4))

        (8  ;; equals
         (let* ((mode (decode-adr-mode opcode 2))
                (op1 (op-param icode (+ pc 1) (car mode)))
                (op2 (op-param icode (+ pc 2) (cadr mode)))
                (dest (svref icode (+ pc 3))))
           (when *trace-intcode*
             (format t "~4A EQ(~A) ~A ~A ~A~%" pc opcode op1 op2 dest))
           (setf (svref icode dest) (if (= op1 op2) 1 0)))
         (incf pc 4))

        (99 ;; EXIT
         (when *trace-intcode*
           (format t "~4A EXIT(~A)~%" pc opcode))
         (return-from run-intcode icode))

        (otherwise
          (format t "Bad opcode ~A at address ~A.~%" (svref icode pc) pc)
          (error 'run-intcode))))))

(defun run (program)
  "Runs the program non-destructively"
  (run-intcode (copy-seq program)))

;;; Unit tests for icode interpreter
(unless 
  (and (equalp (run (parse-intcode "1,0,0,0,99")) #(2 0 0 0 99))
       (equalp (run (parse-intcode "2,3,0,3,99")) #(2 3 0 6 99))
       (equalp (run (parse-intcode "2,4,4,5,99,0")) #(2 4 4 5 99 9801))
       (equalp (run (parse-intcode "1,1,1,4,99,5,6,0,99")) #(30 1 1 4 2 5 6 0 99)))
  (format t "Check your math"))


(defvar prog5 (parse-intcode "3,225,1,225,6,6,1100,1,238,225,104,0,1102,68,5,225,1101,71,12,225,1,117,166,224,1001,224,-100,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1001,66,36,224,101,-87,224,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1101,26,51,225,1102,11,61,224,1001,224,-671,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,59,77,224,101,-136,224,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,1101,11,36,225,1102,31,16,225,102,24,217,224,1001,224,-1656,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,101,60,169,224,1001,224,-147,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,38,69,225,1101,87,42,225,2,17,14,224,101,-355,224,224,4,224,102,8,223,223,1001,224,2,224,1,224,223,223,1002,113,89,224,101,-979,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1102,69,59,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,1007,226,226,224,1002,223,2,223,1006,224,344,1001,223,1,223,1108,226,677,224,102,2,223,223,1005,224,359,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,404,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,434,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,449,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,464,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,479,101,1,223,223,1007,226,677,224,102,2,223,223,1006,224,494,101,1,223,223,107,677,677,224,102,2,223,223,1005,224,509,101,1,223,223,108,677,677,224,102,2,223,223,1006,224,524,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,539,101,1,223,223,107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,569,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,584,1001,223,1,223,1108,226,226,224,102,2,223,223,1005,224,599,1001,223,1,223,1107,677,677,224,1002,223,2,223,1006,224,614,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,629,1001,223,1,223,108,226,226,224,102,2,223,223,1005,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226"))

