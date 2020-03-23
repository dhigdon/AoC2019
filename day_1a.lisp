;;; Advent of Code, Day 1
;;; Module fuel mass is mass/3, rounded down, minus 2

(defun calc-fuel-mass (module-mass)
  (- (floor module-mass 3) 2))

(defun calc-mission-mass (fname)
  (let ((total 0))
    (with-open-file (s fname)
      (do ((l (read-line s) (read-line s nil)))
        ((null l) total)
        (when (string l)
          (let* ((mass (parse-integer l))
                 (fuel (calc-fuel-mass mass)))
            (format t "Mass = ~A, Fuel = ~A~%" mass fuel)
            (incf total fuel)))))
    (format t "Total fuel = ~A" total)))

