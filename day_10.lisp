;;;; Day 10

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

(defparameter *test-1* #P"day_10.1.txt")  ;; (3 4) 8
(defparameter *test-2* #P"day_10.2.txt")  ;; (5 8) 33
(defparameter *test-3* #P"day_10.3.txt")  ;; (1 2) 35
(defparameter *test-4* #P"day_10.4.txt")  ;; (6 3) 41
(defparameter *test-5* #P"day_10.5.txt")  ;; (11 13) 210
(defparameter *problem* #P"day_10.txt")   ;; that's the question....


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

          ;; tabulate
          (dotimes (col dim)
            (setf (aref field row col)
                  (char= #\# (schar data col)))))))))

(defun load-field-graph (fname)
  "Load field data from the given pathname.
  Returns a list of coordinates with asteroids and the field size.
  Coordinates are (x . y)"
  (let ((field nil))
    (with-open-file (str fname)
      (do ((line (read-line str) (read-line str nil :eof))
           (row 0 (1+ row)))
        ((eq line :eof)
         (values field row))
        (let ((data (string-right-trim '(#\Newline #\Return) line)))
          (dotimes (col (length data))
            (when (char= #\# (aref data col))
              (push (cons col row) field))))))))

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

(defun make-vect (origin point)
  "Returns vector (dx . dy) from origin to point.
  Vector is 'normalized' by reducing the fraction it represents."
  (let* ((dx (- (car point) (car origin)))
         (dy (- (cdr point) (cdr origin)))
         (div (gcd dx dy)))
    (cons (/ dx div) (/ dy div))))

(defun apply-vect (vect point)
  (cons (+ (car vect) (car point))
        (+ (cdr vect) (cdr point))))

(defun inside-field-p (coord field-width)
  (and (>= (car coord) 0)
       (>= (cdr coord) 0)
       (<  (car coord) field-width)
       (<  (cdr coord) field-width)))

(defun get-visible (coord field width)
  (let ((working (remove coord (copy-seq field) :test #'equal)))
    (dotimes (radius width working)
      ;(format t "Ping radius ~A~%" radius)
      (iter-sweep
        (car coord) (cdr coord) radius
        #'(lambda (x y)
            (let ((pt (cons x y)))
              (when (member pt working :test #'equal)
                (let ((vect (make-vect coord pt)))
                  ;(format t "Star ~A Slope ~A~%" pt vect)
                  (do ((probe (apply-vect vect pt)
                              (apply-vect vect probe)))
                    ((not (inside-field-p probe (1+ width))))
                    ;(when (member probe working :test #'equal)
                    ;  (format t "Blocks LOS to ~A~%" probe))
                    (setq working (remove probe working :test #'equal)))))))))))


(defparameter test (load-field-graph *test-1*))
(defparameter coord (cons 3 4))

(defun day10 (dataset)
  (let ((best-val 0)
        (best-star nil))
  (multiple-value-bind (field width) (load-field-graph dataset)
    (dolist (star field (values best-star best-val))
      (let ((visible (length (get-visible star field width))))
        (when (> visible best-val)
          (setq best-val visible
                best-star star)))))))

