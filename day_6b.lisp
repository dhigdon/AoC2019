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

;; BODIES hashes a body to a list of its direct moons
(defparameter *bodies* (make-hash-table))

(defun get-moons (world) (gethash world *bodies*))

;; ORBITS hashes a moon to the body it orbits
(defparameter *orbits* (make-hash-table))

(defun get-world (moon) (gethash moon *orbits*))

;; Loading
(defun parse-orbit (spec)
  "Split 'A)B' into (values A B) where A and B are any two symbols"
  (let ((mark (position #\) spec)))
    (when mark
      (values (intern (subseq spec 0 mark)) 
              (intern (subseq spec (1+ mark)))))))

(defun load-orbits (fname)
  "Read filename, which constains a series of lines that
  describe the orbit of one body around another.
  Fills the *bodies* and *orbits* hashtables, and
  returns the number of orbits read."
  (with-open-file (s fname)
    (setf *bodies* (make-hash-table)
          *orbits* (make-hash-table))
    (do* ((l (read-line s) (read-line s nil))
          (count 0 (1+ count)))
      ((null l) count)
      (setq l (string-right-trim '(#\Newline #\Return) l))
      (multiple-value-bind (world moon) (parse-orbit l)
        (setf (gethash moon *orbits*) world)
        (pushnew moon (gethash world *bodies*))))))

;; Debugging function
(defun describe-orbits (world)
  "Returns the tree (as above) for all moons orbiting this world, and their moons if any"
  (cons world (mapcar #'describe-orbits (get-moons world))))


;; The problem - count the orbital "depth" of each body
;; A recursive solution seems easiest:

(defun +depth+ (world level)
  "Find the depth of the world tree starting from 'world'."
  (reduce #'+ (mapcar #'(lambda (m) (+depth+ m (1+ level)))
                      (get-moons world))
          :initial-value level))

(defun depth (world) (+depth+ world 0))


;; Part 2 - find orbital transfer path from one world to the next
;; Solution:
;;  Find if you can transfer "down" the tree
;;  If not, go up one and try again.

;; Find a path to one of a world's moons
;; Returns list of the path or nil of none found
(defun find-moon (target world)
  (if (eq target world)
    (list world)
    (let ((path 
            (some #'(lambda (moon) (find-moon target moon))
                  (get-moons world))))
      (when path (cons world path)))))

(defun find-path (target from)
  "Find the path from->target, including those endpoints.
  Returns nil if no path can be found.
  Ignored worlds visited."
  (cond ((eq from target) (list target))
        ((find-moon target from))
        ((null (get-world from)) nil)
        (t (let ((result (find-path target (get-world from))))
             (when result (cons from result))))))


;; test data
(defvar *test-1* #P"day_6a_test.txt")

;; COM is always the top level world
(defun day-6a () 
  (load-orbits *test-1*)
  (depth 'com))

(defun day-6b ()
  (load-orbits #P"day_6.txt")
  ;; the number of transfers is one less than the number of worlds
  (1- (length (find-path (get-world 'san) (get-world 'you)))))

