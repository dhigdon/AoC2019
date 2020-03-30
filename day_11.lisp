;;; Day 11 - painting robot
;;; by Dan Higdon

(load "intcode")

;; Robot
(defstruct robot
  (x     0 :type fixnum)
  (y     0 :type fixnum)
  (max-x 0 :type fixnum)
  (max-y 0 :type fixnum)
  (min-x most-positive-fixnum :type fixnum)
  (min-y most-positive-fixnum :type fixnum)
  (dir   0 :type (integer 0 3)))

(defun turn-robot (robot facing)
  "Cause the robot to change direction.
  0 means 90 degrees left
  1 means 90 degrees right"
  ;; We use a 4-value direction system
  (let ((dir (+ (robot-dir robot)
                (case facing
                  (0 -1) ;; Left
                  (1  1) ;; right
                  (otherwise 0)
                  ))))
    (setf (robot-dir robot)
          (mod dir 4))))

(defun advance-robot (robot)
  "Move the robot forward 1 unit in its current direction"
  (case (robot-dir robot)
    (0 (decf (robot-y robot)))
    (1 (incf (robot-x robot)))
    (2 (incf (robot-y robot)))
    (3 (decf (robot-x robot))))
  ;; Tabulate the farthest reaches the robot went
  (setf (robot-max-x robot) (max (robot-max-x robot) (robot-x robot)))
  (setf (robot-max-y robot) (max (robot-max-y robot) (robot-y robot)))
  (setf (robot-min-x robot) (min (robot-min-x robot) (robot-x robot)))
  (setf (robot-min-y robot) (min (robot-min-y robot) (robot-y robot))))

(defun paint-robot (robot color panel)
  (let* ((x (robot-x robot))
         (y (robot-y robot))
         (old-color (aref panel y x)))
    (cond ((= old-color color) nil)
          (t (setf (aref panel y x) color)))))

(defun scan-robot (robot panel)
  (let* ((x (robot-x robot))
         (y (robot-y robot))
         (color (aref panel y x)))
    (if (= color 1) 1 0)))

(defun render-panel (panel)
  (dotimes (row (array-dimension panel 0))
    (dotimes (col (array-dimension panel 1))
      (format t "~[ ~;#~; ~]" (aref panel row col)))
    (format t "~%")))

(defun make-panel (width height)
  "Initial value of '2' is the same as black"
  (make-array (list height width) :initial-element 2))

(defun count-updated (panel)
  (let ((count 0))
    (dotimes (row (array-dimension panel 0))
      (dotimes (col (array-dimension panel 1))
        (unless (= 2 (aref panel row col))
          (incf count))))
    count))


(defparameter *program* (load-intcode #P"day_11.txt"))

(defun run-robot (code)
  (let ((interp (make-interp :memory (copy-intcode code 4096)))
        (panel (make-panel 48 8)) ;; TODO
        (robot (make-robot :x 0 :y 0))
        )
    ;; First position starts off black, not "unassigned"
    (paint-robot robot 1 panel)
    (loop 
      (interp-add-input interp (scan-robot robot panel))
      (interp-eval interp)
      (when (interp-has-output interp)
        (let* ((color (interp-read-output interp))
               (turn  (interp-read-output interp)))
          (paint-robot robot color panel)
          (turn-robot robot turn)
          (advance-robot robot)))
      (when (eq 'exit (interp-halted interp)) (return)))
    (render-panel panel)
    (format t "Robot final position <~A, ~A>, facing ~A~%"
            (robot-x robot) (robot-y robot) (robot-dir robot))
    (format t "~A panels changed color~%" (count-updated panel))
    (format t "~A" robot)
    ))

