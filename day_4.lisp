;;; Day 4 - code breaking
;;;    It is a six-digit number.
;;;    The value is within the range given in your puzzle input.
;;;    Two adjacent digits are the same (like 22 in 122345).
;;;    Going from left to right, the digits never decrease;
;;;    they only ever increase or stay the same (like 111123 or 135679).
;;;
;;; Additional rule
;;;   Sequences LONGER than 2 are allowed, but don't count as the double
;;;   for acceptance. So 111123 failes, but 111122 succeed, since it still
;;;   gets that double 22 in there.

(defun validate (digits)
  (let ((run 0) (doubles nil))
    (declare (fixnum run) )
    
    (do ((cur (car digits) (car rest))
         (lst nil cur)
         (rest (cdr digits) (cdr rest)))
      ((null cur))

      ;(format t "~%~4A ~4a ~4a ~4a" lst cur run doubles)
      (cond ((null lst) (incf run))

            ;; Same digit - increase run count
            ((char-equal cur lst) (incf run))

            ;; Ascending digit encountered
            ((char-greaterp cur lst)
             (when (= run 2) (setf doubles t)) 
             (setf run 1))

            ;; error case - descending digits
            (t (return-from validate nil))))

    ;; Still need to process runs in the last digits
    (when (= run 2) (setf doubles t))

    ;; We succeed if we found our doubles
    doubles))

(defun valid-p (pwd)
  (let ((digits (coerce (format nil "~A" pwd) 'list)))
    (validate digits)))

;;; test framework and data
(defconstant start 347312)
(defconstant end 805915)

(defun gen-pwds ()
  (let ((pwds
          (loop for pwd from start to end
                when (valid-p pwd)
                collect pwd)))
    (values (length pwds) pwds)))

