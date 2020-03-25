;;; Day 8 - image decoding
;;;

(defparameter *width* 25)
(defparameter *height* 6)
(defparameter *layer-size* (* *width* *height*))

(defparameter *data* 
  (with-open-file (stream #P"day_8.txt") (read-line stream)))

(defun split-by (seq span)
  "Split seq into a list of span elements.
  The length of seq must be a multiple of span, or the call will fail."
  ;; TODO return (value result rest)?
  (labels ((run (start end result)
                (if (< start end)
                  (run (+ start span) end
                       (cons (subseq seq start (+ start span))
                             result))
                  (nreverse result))))
    (run 0 (length seq) nil)))

(defun decode-char (c)
  (- (char-code c) (char-code #\0)))

(defun eval-layer (layer)
  (let ((result (make-array 3)))
    (loop for c across layer do
          (incf (aref result (decode-char c))))
    result))

(defun validate-data ()
  (let* ((layers (split-by *data* *layer-size*))
         (best (do ((best-count *layer-size* best-count)
                    (best-layer nil best-layer)
                    (best-stats nil best-stats)
                    (l layers (cdr l))
                    (c (mapcar #'eval-layer layers) (cdr c)))
                 ((null l) best-stats)
                 (let ((zeroes (aref (car c) 0)))
                   (when (< zeroes best-count)
                     (setq best-layer (car l)
                           best-stats (car c)
                           best-count zeroes))))))
    (* (aref best 1) (aref best 2))))


(defun combine (a b)
  "The layer combination rule says the highest non-transparent color wins.
  2 is the transparaent color. Apply this function between all pixels of
  a column in the layer stack, from highest to lowest."
  (case a (#\2 b) (otherwise a)))

(defun to-pixel (c) (case c (#\2 #\Space) (#\1 #\#) (#\0 #\.)))

(defun render (img)
  (dotimes (idx *layer-size*)
    (when (and (> idx 0) (zerop (mod idx *width*)))
      (format t "~%"))
    (format t "~C" (to-pixel (aref img idx)))))

(defun day8 ()
  (let* ((layers (split-by *data* *layer-size*))
         (result (copy-seq (car layers))))
    (dolist (layer (cdr layers))
      (dotimes (idx *layer-size*)
        (setf (schar result idx)
              (combine (schar result idx) (schar layer idx)))))
    (render result)))

