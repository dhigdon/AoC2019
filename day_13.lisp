;;; Day 13 - Arcade Cabinet!
(load "intcode")

(defun count-blocks (screen blk)
  (declare (fixnum blx))
  (let ((total 0))
    (declare (fixnum total))
    (dotimes (y (array-dimension screen 0))
      (dotimes (x (array-dimension screen 1))
        (when (= (aref screen y x) blk) (incf total))))
    total))

(defun cls ()
  (format t "~c[2J" (code-char 27)))

(defun render (screen)
  (dotimes (y (array-dimension screen 0))
    (dotimes (x (array-dimension screen 1))
      (format t "~[ ~;|~;#~;-~;*~]" (aref screen y x)))
    (format t "~%")))

(defun play (program)
  "Returns the required screen size and number of tiles set"
  (let* ((interp (make-interp :memory (copy-intcode program 4096)))
         (screen-width 48)
         (screen-height 32)
         (screen (make-array (list screen-height screen-width) :initial-element 0))
         (ball-x   0)
         (paddle-x 0)
         (score 0))
    (declare (fixnum ball-x paddle-x screen-width screen-height)
             (integer score))

    ; This turns the game "on"
    (interp-poke interp 0 2)
    (loop
      (interp-eval interp)

      ; Handle the output as it's generated.
      ; Handle ALL the output from this step
      (loop
        (unless (interp-has-output interp) (return))
        (let ((x (interp-read-output interp))
              (y (interp-read-output interp))
              (tile (interp-read-output interp)))
          (declare (fixnum x y) (integer tile))
          (cond ((and (= x -1) (= y 0))
                 (setf score tile))
                ((= tile 3) ;; paddle
                 (setf paddle-x x
                       (aref screen y x) tile))
                ((= tile 4) ;; Ball
                 (setf ball-x x
                       (aref screen y x) tile))
                (t
                  (setf (aref screen y x) tile)))))

      ; (render screen)
      ;(format t "Ball ~A, Paddle ~A, Score ~A~%" ball-x paddle-x score)

      ; If the program halted, we're don
      (when (eq 'exit (interp-halted interp)) (return))

      ; Likewise, when no blocks remain, we are done
      (when (zerop (count-blocks screen 2)) (return))

      ; Get new input - just track the ball with the paddle
      (interp-add-input interp
                        (cond ((> paddle-x ball-x)  -1)
                              ((< paddle-x ball-x)  1)
                              (t 0)))
      )

    ; Game over - return the score
    score))

(defparameter *program* (load-intcode #P"day_13.txt"))

