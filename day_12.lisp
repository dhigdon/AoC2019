;;; Day 12

(defstruct pt
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum))

(defun pt-eql (a b)
  (and (= (pt-x a) (pt-x b))
       (= (pt-y a) (pt-y b))
       (= (pt-z a) (pt-z b))))


(defstruct moon
  (pos (make-pt) :type pt)
  (vel (make-pt) :type pt))

(defun moon-vel-eql (a b)
  (declare (moon a b))
  (pt-eql (moon-vel a) (moon-vel b)))

(defun moon-pos-eql (a b)
  (declare (moon a b))
  (pt-eql (moon-pos a) (moon-pos b)))

(defun moon-eql (a b)
  (declare (moon a b))
  (and (moon-pos-eql a b)
       (moon-vel-eql a b)))

(defun deep-copy-moon (moon)
  (declare (moon moon))
  (make-moon :pos (copy-pt (moon-pos moon))
             :vel (copy-pt (moon-vel moon))))


(defun apply-gravity (a b)
  "A and B attract eachother"
  (declare (moon a b))
  (let ((pa (moon-pos a))
        (pb (moon-pos b))
        (va (moon-vel a))
        (vb (moon-vel b)))
    (declare (pt pa pb va vb))
    (let ((dx (- (pt-x pa) (pt-x pb)))
          (dy (- (pt-y pa) (pt-y pb)))
          (dz (- (pt-z pa) (pt-z pb))))
      (declare (fixnum dx dy dz))
      (cond ((< dx 0) (incf (pt-x va)) (decf (pt-x vb)))
            ((> dx 0) (decf (pt-x va)) (incf (pt-x vb))))
      (cond ((< dy 0) (incf (pt-y va)) (decf (pt-y vb)))
            ((> dy 0) (decf (pt-y va)) (incf (pt-y vb))))
      (cond ((< dz 0) (incf (pt-z va)) (decf (pt-z vb)))
            ((> dz 0) (decf (pt-z va)) (incf (pt-z vb)))))))

(defun attract-moons (moons)
  (do ((moon (car moons) (car rest))
       (rest (cdr moons) (cdr rest)))
    ((null rest) moons)
    (dolist (target rest)
      (apply-gravity moon target))))

(defun apply-velocity (moon)
  (let ((v (moon-vel moon))
        (p (moon-pos moon)))
    (declare (pt v p))
    (incf (pt-x p) (pt-x v))
    (incf (pt-y p) (pt-y v))
    (incf (pt-z p) (pt-z v))
    moon))

(defun update-moons (moons)
  (attract-moons moons)
  (mapc #'apply-velocity moons))

(defun potential-energy (moon)
  (let ((p (moon-pos moon)))
    (+ (abs (pt-x p)) (abs (pt-y p)) (abs (pt-z p)))))

(defun kinetic-energy (moon)
  (let ((p (moon-vel moon)))
    (+ (abs (pt-x p)) (abs (pt-y p)) (abs (pt-z p)))))

(defun calc-energy (moon)
  (* (potential-energy moon) (kinetic-energy moon)))

;;; Test data
(defparameter *test1*
  (list (make-moon :pos (make-pt :x -1 :y   0 :z  2))
        (make-moon :pos (make-pt :x  2 :y -10 :z -7))
        (make-moon :pos (make-pt :x  4 :y  -8 :z  8))
        (make-moon :pos (make-pt :x  3 :y   5 :z -1))
        ))

(defparameter *test2*
  (list (make-moon :pos (make-pt :x -8 :y -10 :z 0))
        (make-moon :pos (make-pt :x  5 :y  5 :z 10))
        (make-moon :pos (make-pt :x  2 :y -7 :z 3))
        (make-moon :pos (make-pt :x  9 :y -8 :z -3))
        ))


;;; Problem data
;<x=-10, y=-13, z=7>
;<x=1, y=2, z=1>
;<x=-15, y=-3, z=13>
;<x=3, y=7, z=-4>

(defparameter *data*
  (list (make-moon :pos (make-pt :x -10 :y -13 :z 7))
        (make-moon :pos (make-pt :x   1 :y   2 :z 1))
        (make-moon :pos (make-pt :x -15 :y  -3 :z 13))
        (make-moon :pos (make-pt :x   3 :y   7 :z -4))
        ))

(defun day12a (data)
  (let ((working (mapcar #'deep-copy-moon data)))
    (dotimes (n 1000) (update-moons working))
    (reduce #'+ (mapcar #'calc-energy working))))

;; NOTE - this does not run in a reasonable time.
;; I wound up going for help, but found that the real
;; solution involves finding cycles in each axis an LCM'ing
;; them together.
;; Consequently, I don't feel like solving this problem
;; for real anymore, because I know the trick now.
(defun day12b (data)
  (let ((working (mapcar #'deep-copy-moon data)))
    (update-moons working)
    (do ((count 1 (1+ count)))
      ((every #'moon-eql working data) count)
      (update-moons working))))


