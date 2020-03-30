;;;; Day 10, alternate version (grid based)
;;; Note that this version is MUCH faster and generates the same results,
;;; even though it uses the same approach. All that differs is that
;;; this version stores the asteroid field in a 2D boolean array, and deletes
;;; asteroids by marking them FALSE.

;;; Asteroid field LOS checks
;;;
;;; The field is a grid, and asteroids are atomically in the center
;;; of the grid.
;;;
;;; Finding visible asteroids is the trick.
;;; Plan:
;;;   for each location where there is an asteroid
;;;      copy the field
;;;      sweep concentric squares around the asteroid
;;;         if there's an asteroid in the location
;;;            use the slope to walk to the edge of the field,
;;;               removing any asteroids you find
;;;         count all asteroids still in the field
;;;      record the asteroid's location and count
;;;
;;; For this implementation, I'll use a 2-d array of booleans,
;;; generated from a data file.
;;;

(defun load-field (fname)
  "Load field data from the given pathname.
  Returns a 2D array with T for every asteroid and NIL for empty space"
  (let ((field))
    (with-open-file (str fname)
      (do ((line (read-line str) (read-line str nil :eof))
           (row 0 (1+ row)))
        ((eq line :eof) field)
        (let* ((data (string-right-trim '(#\Newline #\Return) line))
               (dim  (length data)))

          ;; Fields are symmetrical, so once we know how wide
          ;; the field is, we can allocate storage
          (when (null field)
            (setq field (make-array (list dim dim))))

          ;; tabulate this row of data
          (dotimes (col dim)
            (setf (aref field row col)
                  (char= #\# (schar data col)))))))))

(defun iter-sweep (x y radius func)
  "Call func on every point in the sweep radius around center.
  The points are not generated in any particular of order."
  (let ((left     (- x radius))
        (right    (+ x radius))
        (top      (- y radius))
        (bottom   (+ y radius)))
    (declare (fixnum x y radius left right top bottom))

    ;; To make it easier, we'll sweep top and bottom edges first,
    (loop for x from left to right do
          (funcall func x top)
          (funcall func x bottom))

    ;; Now, iterate left/right edges, not including the corners,
    ;; which were already iterated as part of the top/bottom edges.
    (loop for y from (1+ top) to (1- bottom) do
          (funcall func left  y)
          (funcall func right y))))


(defun make-sweep (center radius)
  "Returns a list of coordinates that surround center and are
  radius units away. The sweep is a square in shape, not a circle."
  (let ((sweep '()))
    (iter-sweep (car center) (cdr center) radius
                #'(lambda (x y) (push (cons x y) sweep)))
    sweep))

(defun make-vect (cx cy px py)
  "Returns vector (values vx vy) from origin to point.
  Vector is 'normalized' by reducing the fraction it represents."
  (declare (fixnum cx cy px py))
  (let* ((dx (- px cx))
         (dy (- py cy))
         (div (gcd dx dy)))
    (values (floor dx div) (floor dy div))))

(defun get-visible (cx cy field)
  "Return (values visible-list visible-count) from a position (cx cy)
  inside the given field."
  (declare (fixnum cx cy))
  (let* ((dim     (array-dimensions field))
         (rows    (car dim))
         (cols    (cadr dim))
         (width   (max rows cols))
         (working (make-array dim))
         (visibles nil)
         (visible-count 0))
    (declare (fixnum visible-count rows cols width))

    ;; Copy our field, minus the coordinate we're scanning for
    (dotimes (row rows)
      (dotimes (col cols)
        (setf (aref working row col) (aref field row col))))
    (setf (aref working cy cx) nil)

    ;; Sweep a wavefront out to the full width of the field,
    ;; shadowing any asteroids in question.
    (dotimes (radius width)
      ;(format t "Ping radius ~A~%" radius)
      (iter-sweep
        cx cy radius
        #'(lambda (x y)
            (declare (fixnum x y))
            (when (and (array-in-bounds-p working x y)
                       (aref working y x))
              (multiple-value-bind (vx vy) (make-vect cx cy x y)
                (do ((px (+ vx x) (+ vx px))
                     (py (+ vy y) (+ vy py)))
                  ((not (array-in-bounds-p working px py )))
                  (setf (aref working py px) nil)))))))

    ;; finally, collect all the stars that are still present
    (dotimes (row rows)
      (dotimes (col cols)
        (when (aref working row col)
          (incf visible-count)
          (push (cons col row) visibles))))
    (values visibles visible-count)))

;;; -------------------------------------------------------------------

(defparameter *test-1* #P"day_10.1.txt")  ;; (3 4) 8
(defparameter *test-2* #P"day_10.2.txt")  ;; (5 8) 33
(defparameter *test-3* #P"day_10.3.txt")  ;; (1 2) 35
(defparameter *test-4* #P"day_10.4.txt")  ;; (6 3) 41
(defparameter *test-5* #P"day_10.5.txt")  ;; (11 13) 210
(defparameter *problem* #P"day_10.txt")   ;; that's the question....

(defparameter test (load-field *test-1*))
(defparameter coord (cons 3 4))

(defun day10 (dataset)
  (let ((best-val 0)
        (best-star nil))
    (let ((field (load-field dataset)))
      (dotimes (y (array-dimension field 0))
        (dotimes (x (array-dimension field 1))
          (when (aref field y x)
            (let ((visible (nth-value 1 (get-visible x y field))))
              (when (> visible best-val)
                (setq best-val visible
                      best-star (cons x y))))))))
    (values best-star best-val)))

