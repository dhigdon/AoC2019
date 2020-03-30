;;; INTCODE interpreter

(require 'hadt)

;;; INTCODE reading

(defun parse-intcode (s)
  "Parse a string of comma separated integers into a simple vector of those integers"
  (declare (simple-string s))
  (let* ((slen (length s))
         (code (make-array slen :element-type 'integer :adjustable t :fill-pointer 0)))
    (declare (fixnum slen) ((vector integer) code))
    (do
      ((idx 0 idx))
      ((>= idx slen)
       (copy-seq code))
      (multiple-value-bind (op next) (parse-integer s :start idx :junk-allowed t)
        (vector-push-extend op code (- slen idx))
        (setf idx (1+ next))))))

(defun load-intcode (filename)
  "Read the first line of code in the file and parse it as an INTCODE program"
  (with-open-file (s filename)
    (parse-intcode (read-line s))))

;; INTCODE CPU

;;; Addressing mode
(deftype adr-mode () `(ftype (function (simple-vector fixnum fixnum) fixnum)))

;; Implement addressing mode operations
(declaim (adr-mode adr-mode-pos adr-mode-imm adr-mode-rel))
(defun adr-mode-pos (mem val base) (progn base (svref mem val)))
(defun adr-mode-imm (mem val base) (progn mem base val))
(defun adr-mode-rel (mem val base) (svref mem (+ base val)))

(declaim (adr-mode dst-mode-pos dst-mode-rel))
(defun dst-mode-pos (mem val base) mem base val)
;; note, there is no immediate destination mode
(defun dst-mode-rel (mem val base) mem (+ base val))

(defun opcode-adr-mode (opcode inlen outlen)
  (declare (fixnum opcode inlen outlen))
  (let ((result (make-array (+ inlen outlen)))
        (modes (floor opcode 100))
        (mode 0))
    ;; Decode the adr modes
    (dotimes (i inlen)
      (multiple-value-setq (modes mode) (floor modes 10))
      (setf (svref result i)
            (case mode
              (0 #'adr-mode-pos)
              (1 #'adr-mode-imm)
              (2 #'adr-mode-rel)
              (otherwise (error "~A is not a valid adr-mode" mode)))))
    ;; Now, decode the dst modes
    (dotimes (i outlen)
      (multiple-value-setq (modes mode) (floor modes 10))
      (setf (svref result (+ i inlen))
            (case mode
              (0 #'dst-mode-pos)
              (2 #'dst-mode-rel)
              (otherwise (error "~A is not a valid dst-mode" mode)))))
    (if (zerop modes)
      result
      (error "Bad adr-mode-decode ~A" opcode))))

(defparameter *trace-intcode* nil)

;; The running state of the interpreter
(defstruct interp
  (memory   #()   :type (array integer))
  (pc       0     :type fixnum)
  (base     0     :type fixnum)
  (halted   nil   :type (or nil symbol))
  (input    (hadt:make-queue))
  (output   (hadt:make-queue)))

(declaim (ftype (function (interp) fixnum) interp-opcode))
(defun interp-opcode (interp)
  "Fetch the opcode at the current PC"
  (svref (interp-memory interp)
         (interp-pc interp)))

(declaim (ftype (function (interp fixnum) fixnum) interp-advance interp-jump))
(defun interp-advance (interp n) (incf (interp-pc interp) n))
(defun interp-jump (interp n) (setf (interp-pc interp) n))

(defun interp-param (interp index modes)
  "Return the value of the zero-indexed opcode parameter, in light of the
  addressing mode supplied"
  (declare (interp interp) (fixnum index) (simple-vector modes))
  (let ((mem (interp-memory interp))
        (adr (+ 1 (interp-pc interp) index)))
    (funcall (svref modes index)
             mem (svref mem adr) (interp-base interp))))

(defun interp-set-mem (interp address value)
  "Store the value in the location the opcode's field at index indicates.
  Note that destinations are always addresses, never immediate values,
  and values can be bignums or any integer."
  (declare (interp interp) (fixnum address) (integer value))
  (setf (svref (interp-memory interp) address) value))

;;; Input is handled by way of a queue
(defun interp-add-input (interp val)   (hadt:enqueue val (interp-input interp))) 
(defun interp-read (interp)            (hadt:dequeue (interp-input interp))) 
(defun interp-has-input (interp)       (not (hadt:queue-empty-p (interp-input interp)))) 

(defun interp-write (interp val)       (hadt:enqueue val (interp-output interp))) 
(defun interp-read-output (interp)     (hadt:dequeue (interp-output interp)))
(defun interp-has-output (interp)      (not (hadt:queue-empty-p (interp-output interp))))

(defun interp-trace (interp name inlen outlen)
  "Disassemble the current opcode, given a name and the operand counts"
  (when *trace-intcode*
    (format t "~4A ~A(~A)" (interp-pc interp) name (interp-opcode interp))
    (let ((mode (opcode-adr-mode (interp-opcode interp) inlen outlen)))
      (dotimes (i inlen)  (format t " ~A" (interp-param interp i mode)))
      (dotimes (i outlen) (format t " [~A]" (interp-param interp (+ i inlen) mode))))
    (format t "~%")))

(defun interp-retire (interp name inlen outlen dest val)
  "Retire the current opcode named 'name and advance the PC to the next opcode.
  inlen = the number of input fields in the opcode
  outlen = the number of output fields (either 0 or 1)
  dest = address to store the opcode's result, or nil of no value is written
  val  = the opcode's value"
  (declare (interp interp) (string name) (fixnum inlen outlen dest) (integer val))
  (interp-trace interp name inlen outlen)
  (when (> outlen 0) (interp-set-mem interp dest val))
  (interp-advance interp (+ 1 inlen outlen)))


(defun interp-step (interp)
  "Advance the interpreter through one instruction"
  (declare (interp interp))
  (let ((opcode (interp-opcode interp))
        (pc (interp-pc interp)))
    (declare (fixnum opcode pc))
    (case (mod opcode 100)
      (1  ;; ADD
       ;; I am sure that there is a way to abstract this pattern,
       ;; but this is good enough for now. Knowing me, I'll wind
       ;; up replacing this with something more "macro-y" in the future.
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
        (error "Bad opcode ~4A ~A~%" pc opcode)))))

(defun interp-eval (interp)
  "Run the interpreter until it halts"
  (unless (eq (interp-halted interp) 'exit)
    (loop
      (interp-step interp)
      (when (interp-halted interp)
        (return)))))

(defun copy-intcode (icode memsize)
  "Create a copy of icode, possibly expanded to memsize extra bytes.
  Copied image does not include a fill pointer or the ability to resize."
  (declare (simple-vector icode) (fixnum memsize))
  (let ((result (make-array (max (length icode) memsize))))
    (dotimes (idx (length icode) result)
      (setf (aref result idx) (aref icode idx)))))

(defun interp-run (icode &key (memsize 2048))
  "Run the intcode in the provided vector, and return a new
  memory image with the results."
  (let ((interp (make-interp :memory (copy-intcode icode memsize))))
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

