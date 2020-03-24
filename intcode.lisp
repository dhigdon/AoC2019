(require 'hadt)

(defun parse-intcode (s)
  "Parse a string of comma separated integers into a simple vector of those integers"
  (let ((code (hadt:make-queue)) 
        (slen (length s))
        (idx 0))
    (declare (fixnum slen idx))
    (loop
      (cond ((< idx slen)
             (multiple-value-bind (op next) (parse-integer s :start idx :junk-allowed t)
               (assert (or (= next slen) (char= #\, (aref s next))))
               (hadt:enqueue op code)
               (setf idx (1+ next))))
            (t (return (coerce (hadt:queue-to-list code) 'vector)))))))

;; Intcode CPU

;;; Addressing mode
(deftype adr-mode ()
  `(ftype (function (simple-vector fixnum) fixnum)))

;; Implement addressing mode operations
(declaim (adr-mode adr-mode-pos adr-mode-imm))
(defun adr-mode-pos (mem val) (svref mem val)) 
(defun adr-mode-imm (mem val) (progn mem val))

(defun opcode-adr-mode (opcode len)
  (let ((result (make-array (1+ len)))
        (modes (floor opcode 100))
        (mode 0))
    (setf (svref result len) #'adr-mode-imm)
    (dotimes (i len result)
      (multiple-value-setq (modes mode) (floor modes 10))
      (setf (svref result i)
            (case mode (0 #'adr-mode-pos) (1 #'adr-mode-imm))))))

(defparameter *trace-intcode* nil)

;; The running state of the interpreter
(defstruct interp
  (memory   #()   :type simple-vector)
  (pc       0     :type fixnum)
  (halted   nil   :type (or nil symbol))
  (input    (hadt:make-queue))
  (output   (hadt:make-queue)))


(defun interp-opcode (interp)
  "Fetch the opcode at the current PC"
  (svref (interp-memory interp)
         (interp-pc interp)))

(defun interp-advance (interp n) (incf (interp-pc interp) n))
(defun interp-jump (interp n) (setf (interp-pc interp) n))

(defun interp-param (interp index modes)
  "Return the value of the zero-indexed opcode parameter, in light of the
  addressing mode supplied"
  (let ((mem (interp-memory interp))
        (adr (+ 1 (interp-pc interp) index)))
    (funcall (svref modes index) mem (svref mem adr))))

(defun interp-set-mem (interp address value)
  "Store the value in the location the opcode's field at index indicates.
  Note that destinations are always addresses, never immediate values"
  (setf (svref (interp-memory interp) address) value))

(defun interp-add-input (interp val)
  (hadt:enqueue val (interp-input interp)))

(defun interp-read (interp)
  (hadt:dequeue (interp-input interp)))

(defun interp-has-input (interp)
  (not (hadt:queue-empty-p (interp-input interp))))

(defun interp-write (interp val)
  (hadt:enqueue val (interp-output interp)))

(defun interp-read-output (interp)
  (hadt:dequeue (interp-output interp)))

(defun interp-trace (interp name len)
  (when *trace-intcode*
    (format t "~4A ~A" (interp-pc interp) name)
    (let ((mode (opcode-adr-mode (interp-opcode interp) len)))
      (dotimes (i len)
        (format t " ~A" (interp-param interp i mode)))
      (format t " [~A]" (interp-param interp len mode)))
    (format t "~%")))

(defun interp-retire (interp name len dest val)
  (interp-trace interp name len)
  (interp-set-mem interp dest val)
  (interp-advance interp (+ 2 len)))

(defun interp-step (interp)
  "Advance the interpreter through one instruction"
  (let ((opcode (interp-opcode interp))
        (pc (interp-pc interp)))
    (case (mod opcode 100)
      (1  ;; ADD
       (let* ((mode (opcode-adr-mode opcode 2))
              (op1  (interp-param interp 0 mode))
              (op2  (interp-param interp 1 mode))
              (dest (interp-param interp 2 mode))
              (result (+ op1 op2)))
         (interp-retire interp "ADD" 2 dest result)))

      (2  ;; MUL
       (let* ((mode (opcode-adr-mode opcode 2))
              (op1  (interp-param interp 0 mode))
              (op2  (interp-param interp 1 mode))
              (dest (interp-param interp 2 mode))
              (result (* op1 op2)))
         (interp-retire interp "MUL" 2 dest result)))

      (3  ;; READ
       (let* ((mode (opcode-adr-mode opcode 0))
              (dest (interp-param interp 0 mode)))
         ;; Note, read blocks until input is provided
         (if (interp-has-input interp)
           (let ((v (interp-read interp)))
             (setf (interp-halted interp) nil)
             (interp-retire interp (format nil "READ '~A' -> " v) 0 dest v))
           (progn 
             (setf (interp-halted interp) 'input)
             (when *trace-intcode* (format t "~4A READ <blocked>~%" pc))
             pc))))

      (4  ;; WRITE
       (let* ((mode (opcode-adr-mode opcode 1))
              (op1 (interp-param interp 0 mode)))
         (when *trace-intcode* (format t "~4A WRITE ~A~%" pc op1))
         ;; Output always goes into a queue
         ;; higher level code is responsible for delivering this output to
         ;; its intended destination
         (interp-write interp op1)
         (interp-advance interp 2)))

      (5  ;; Jump-if-true
       (let* ((mode (opcode-adr-mode opcode 2))
              (test (interp-param interp 0 mode))
              (targ (interp-param interp 1 mode)))
         (when *trace-intcode* (format t "~4A J (~A <> 0) ~A~%" pc test targ))
         (if (zerop test)
           (interp-advance interp 3)
           (interp-jump interp targ))))

      (6  ;; Jump-if-false
       (let* ((mode (opcode-adr-mode opcode 2))
              (test (interp-param interp 0 mode))
              (targ (interp-param interp 1 mode)))
         (when *trace-intcode* (format t "~4A J (~A == 0) ~A~%" pc test targ))
         (if (zerop test)
           (interp-jump interp targ)
           (interp-advance interp 3))))

      (7  ;; Less-than
       (let* ((mode (opcode-adr-mode opcode 2))
              (op1  (interp-param interp 0 mode))
              (op2  (interp-param interp 1 mode))
              (dest (interp-param interp 2 mode))
              (result (if (< op1 op2) 1 0)))
         (interp-retire interp "LT" 2 dest result)))

      (8  ;; equals
       (let* ((mode (opcode-adr-mode opcode 2))
              (op1  (interp-param interp 0 mode))
              (op2  (interp-param interp 1 mode))
              (dest (interp-param interp 2 mode))
              (result (if (= op1 op2) 1 0)))
         (interp-retire interp "EQ" 2 dest result)))

      (99 ;; EXIT
       (when *trace-intcode* (format t "~4A EXIT(~A)~%" pc opcode))
       (setf (interp-halted interp) 'exit)
       ;; Do not advance past this instruction!
       (interp-pc interp))

      (otherwise
        (format t "Bad opcode ~4A ~A~%" pc opcode)
        (error 'interp-step))))
  )

(defun interp-eval (interp)
  (unless (eq (interp-halted interp) 'exit)
    (loop
      (interp-step interp)
      (when (interp-halted interp)
        (return)))))

(defun interp-run (icode)
  "Run the intcode in the provided vector, and return a new
  memory image with the results."
  (let ((interp (make-interp :memory (copy-seq icode))))
    (loop
      ;; If the program is blocking, process that block
      (case (interp-halted interp)
        (exit (return))
        (input (interp-add-input
                 interp
                 (parse-integer (string-trim " " (read-line))))))

      (interp-step interp)
      (dolist (out (hadt:queue-flatten (interp-output interp)))
        (format t "Out: ~A~%" out)))
    ;; The result is the new memory image
    (interp-memory interp)))

;;;;;;;;
;;;;;;;;

;;; Unit tests for icode interpreter
;(setf *trace-intcode* t)
(unless 
  (and (equalp (interp-run (parse-intcode "1,0,0,0,99")) #(2 0 0 0 99))
       (equalp (interp-run (parse-intcode "2,3,0,3,99")) #(2 3 0 6 99))
       (equalp (interp-run (parse-intcode "2,4,4,5,99,0")) #(2 4 4 5 99 9801))
       (equalp (interp-run (parse-intcode "1,1,1,4,99,5,6,0,99")) #(30 1 1 4 2 5 6 0 99)))
  (format t "Check your math"))


