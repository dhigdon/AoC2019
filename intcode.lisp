(require 'hadt)

(defun parse-intcode (s)
  "Parse a string of comma separated integers into a simple vector of those integers"
  (let ((code (hadt:make-queue)) 
        (slen (length s))
        (idx 0))
    (loop
      (cond ((< idx slen)
             (multiple-value-bind (op next) (parse-integer s :start idx :junk-allowed t)
               (assert (or (= next slen) (char= #\, (aref s next))))
               (hadt:enqueue op code)
               (setf idx (1+ next))))
            (t (return (coerce (hadt:queue-flatten code) 'vector)))))))

;; Intcode CPU

;;; Addressing mode
(deftype adr-mode () `(ftype (function (simple-vector fixnum fixnum) fixnum)))

;; Implement addressing mode operations
(declaim (adr-mode adr-mode-pos adr-mode-imm adr-mode-rel))
(defun adr-mode-pos (mem val base) (progn base (svref mem val)))
(defun adr-mode-imm (mem val base) (progn mem base val))
(defun adr-mode-rel (mem val base) (svref mem (+ base val)))

(declaim (adr-mode dst-mode-pos dst-mode-imm dst-mode-rel))
(defun dst-mode-pos (mem val base) mem base val)
(defun dst-mode-imm (mem val base) mem val base (error 'immediate))
(defun dst-mode-rel (mem val base) mem (+ base val))

(defun opcode-adr-mode (opcode len outlen)
  (let ((result (make-array (+ len outlen)))
        (modes (floor opcode 100))
        (mode 0))
    (dotimes (i len)
      (multiple-value-setq (modes mode) (floor modes 10))
      (setf (svref result i)
            (case mode
              (0 #'adr-mode-pos)
              (1 #'adr-mode-imm)
              (2 #'adr-mode-rel))))
    ;; Now, decode the dst modes
    (dotimes (i outlen)
      (multiple-value-setq (modes mode) (floor modes 10))
      (setf (svref result (+ i len))
            (case mode
              (0 #'dst-mode-pos)
              (1 #'dst-mode-imm)
              (2 #'dst-mode-rel))))
    (unless (zerop modes) (error 'bad-adr-mode-decode))
    result))

(defparameter *trace-intcode* nil)

;; The running state of the interpreter
(defstruct interp
  (memory   #()   :type simple-vector)
  (pc       0     :type fixnum)
  (base     0     :type fixnum)
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
    (funcall (svref modes index)
             mem (svref mem adr) (interp-base interp))))

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

(defun interp-trace (interp name len outlen)
  (when *trace-intcode*
    (format t "~4A ~A(~A)" (interp-pc interp) name (interp-opcode interp))
    (let ((mode (opcode-adr-mode (interp-opcode interp) len outlen)))
      (dotimes (i len)    (format t " ~A" (interp-param interp i mode)))
      (dotimes (i outlen) (format t " [~A]" (interp-param interp (+ i len) mode))))
    (format t "~%")))

(defun interp-retire (interp name len outlen dest val)
  "Retire the current opcode named 'name and advance the PC to the next opcode.
  len = the number of input fields in the opcode
  outlen = the number of output fields (either 0 or 1)
  dest = address to store the opcode's result, or nil of no value is written
  val  = the opcode's value"
  (interp-trace interp name len outlen)
  (when (> outlen 0) (interp-set-mem interp dest val))
  (interp-advance interp (+ 1 len outlen)))

(defun interp-step (interp)
  "Advance the interpreter through one instruction"
  (let ((opcode (interp-opcode interp))
        (pc (interp-pc interp)))
    (case (mod opcode 100)
      (1  ;; ADD
       (let* ((mode (opcode-adr-mode opcode 2 1))
              (op1  (interp-param interp 0 mode))
              (op2  (interp-param interp 1 mode))
              (dest (interp-param interp 2 mode))
              (result (+ op1 op2)))
         (interp-retire interp "ADD" 2 1 dest result)))

      (2  ;; MUL
       (let* ((mode (opcode-adr-mode opcode 2 1))
              (op1  (interp-param interp 0 mode))
              (op2  (interp-param interp 1 mode))
              (dest (interp-param interp 2 mode))
              (result (* op1 op2)))
         (interp-retire interp "MUL" 2 1 dest result)))

      (3  ;; READ
       ;; Note, read blocks until input is provided
       (cond ((interp-has-input interp)
              (setf (interp-halted interp) nil)
              (let* ((mode (opcode-adr-mode opcode 0 1))
                     (dest (interp-param interp 0 mode))
                     (v    (interp-read interp)))
                (interp-retire interp "READ" 0 1 dest v)))
             (t
               ;; Suspend the processor - will only resume when caller provides
               ;; input to the interpreter.
               (setf (interp-halted interp) 'input)
               (interp-trace interp "READ<BLOCKED>" 0 0)
               pc)))

      (4  ;; WRITE
       (let* ((mode (opcode-adr-mode opcode 1 0))
              (op1 (interp-param interp 0 mode)))
         (interp-write interp op1)
         (interp-retire interp "WRITE" 1 0 0 op1)))

      (5  ;; Jump-if-true
       (let* ((mode (opcode-adr-mode opcode 2 0))
              (test (interp-param interp 0 mode))
              (targ (interp-param interp 1 mode)))
         (when *trace-intcode* (format t "~4A J(~A) (~A <> 0) ~A~%" pc opcode test targ))
         (if (zerop test)
           (interp-advance interp 3)
           (interp-jump interp targ))))

      (6  ;; Jump-if-false
       (let* ((mode (opcode-adr-mode opcode 2 0))
              (test (interp-param interp 0 mode))
              (targ (interp-param interp 1 mode)))
         (when *trace-intcode* (format t "~4A J(~A) (~A == 0) ~A~%" pc opcode test targ))
         (if (zerop test)
           (interp-jump interp targ)
           (interp-advance interp 3))))

      (7  ;; Less-than
       (let* ((mode (opcode-adr-mode opcode 2 1))
              (op1  (interp-param interp 0 mode))
              (op2  (interp-param interp 1 mode))
              (dest (interp-param interp 2 mode))
              (result (if (< op1 op2) 1 0)))
         (interp-retire interp "LT" 2 1 dest result)))

      (8  ;; equals
       (let* ((mode (opcode-adr-mode opcode 2 1))
              (op1  (interp-param interp 0 mode))
              (op2  (interp-param interp 1 mode))
              (dest (interp-param interp 2 mode))
              (result (if (= op1 op2) 1 0)))
         (interp-retire interp "EQ" 2 1 dest result)))

      (9 ;; Rebase
       (let* ((mode (opcode-adr-mode opcode 1 0))
              (op1  (interp-param interp 0 mode)))
         (incf (interp-base interp) op1)
         (when *trace-intcode* (format t "~4A REBASE(~A) ~A, base=~A~%" 
                                       pc opcode op1 (interp-base interp)))
         (interp-advance interp 2)))

      (99 ;; EXIT
       (interp-trace interp "EXIT" 0 0)
       (setf (interp-halted interp) 'exit)
       ;; Do not advance past this instruction!
       pc)

      (otherwise
        (format t "Bad opcode ~4A ~A~%" pc opcode)
        (error 'interp-step))))
  )

(defun interp-eval (interp)
  "Run the interpreter until it halts"
  (unless (eq (interp-halted interp) 'exit)
    (loop
      (interp-step interp)
      (when (interp-halted interp)
        (return)))))

(defun interp-run (icode &key (memsize 2048))
  "Run the intcode in the provided vector, and return a new
  memory image with the results."
  (let ((interp (make-interp :memory (adjust-array (copy-seq icode) (max (length icode) memsize)))))
    (loop
      ;; If the program is blocking, process that halted state.
      (case (interp-halted interp)
        (input (interp-add-input interp (parse-integer (string-trim " " (read-line)))))
        (exit  (return)))

      (interp-step interp)
      
      ;; Display output as soon as possible - this is an interactive run
      (dolist (out (hadt:queue-flatten (interp-output interp)))
        (format t "Out: ~A~%" out)))

    ;; The result is the new memory image
    (interp-memory interp)))

;;;;;;;;
;;;;;;;;

;;; Unit tests for icode interpreter
(defun unit-test ()
  (unless 
    (and (equalp (interp-run (parse-intcode "1,0,0,0,99") :memsize 0) #(2 0 0 0 99))
         (equalp (interp-run (parse-intcode "2,3,0,3,99") :memsize 0) #(2 3 0 6 99))
         (equalp (interp-run (parse-intcode "2,4,4,5,99,0") :memsize 0) #(2 4 4 5 99 9801))
         (equalp (interp-run (parse-intcode "1,1,1,4,99,5,6,0,99") :memsize 0) #(30 1 1 4 2 5 6 0 99)))
    (format t "Check your math")))

;(setf *trace-intcode* t)
;(unit-test)

