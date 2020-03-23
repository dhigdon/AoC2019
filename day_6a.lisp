;;; Orbits
;;;
;;; Data format:
;;; A)B means "b is in orbit around a"

;; This is basically building a tree out of connection pairs
;; Find 'world' in the orbits and append the moon direcly to it.

;; Data format is a list, where each world is a list.
;; So a system containt "A)B A)C B)D E)F" is
;; ((A (B (D))
;;     (C))
;;  (E (F)))

;; Loading
(defparameter *universe* (make-hash-table))

(defun parse-orbit (spec)
  "Split 'A)B' into (values A B) where A and B are any two symbols"
  (let ((mark (position #\) spec)))
    (when mark
      (values (intern (subseq spec 0 mark)) 
              (intern (subseq spec (1+ mark)))))))

(defun read-orbits (fname)
  (with-open-file (s fname)
    (do* ((l (read-line s) (read-line s nil))
          (count 0 (1+ count)))
      ((null l) count)
      (setq l (string-right-trim '(#\Newline #\Return) l))
      (multiple-value-bind (world moon) (parse-orbit l)
        ;; Add the moon to the world's orbit list
        (pushnew moon (gethash world *universe*))))))

;; Debugging function
(defun describe-orbits (world)
  "Returns the tree (as above) for all moons orbiting this world, and their moons if any"
  (cons world
        (mapcar #'describe-orbits
                (gethash world *universe*))))


;; The problem - count the orbital "depth" of each body
;; A recursive solution seems easiest:

(defun +depth+ (world level)
  "Find the depth of the world tree starting from 'world'."
  (reduce #'+ (mapcar #'(lambda (m) (+depth+ m (1+ level)))
                      (gethash world *universe*))
          :initial-value level))

(defun depth (world) (+depth+ world 0))


;; test data
(defvar *test-1* #P"day_6a_test.txt")
(defvar *test* #P"day_6.txt")

(defun day-6a () (depth 'com))

