;;;; Day 14 - chemical reactions - array based
;;; Tests show this version is 2.5x faster than the hashtable version,
;;; which points to the crucible lookup being a major bottleneck.
;;; This seems fair, since my hashtable based one sped up when I
;;; split quantity and total into two separate hashtables.
;;;
;;; This version is to be preferred.
;;; The logic is identical to _14, which served as useful prototype.

(require 'cl-utilities)
(use-package 'cl-utilities)

;;; -------------------------------------------------------------------

;;; Data conversion - parse the rules

(defstruct rule
  (reactant nil :type (or symbol fixnum))
  (quantity 0   :type integer))

(defun parse-rule (num name)
  "Make a rule from the two strings provided"
  (declare (string num name))
  (make-rule
    :reactant (intern name)
    :quantity (parse-integer num)))

(defun tokenize (s)
  (declare (string s))
  (split-sequence #\Space (remove #\, s)))

(defun parse-reaction (s)
  "Takes an input string of the form
  <int> <id>[, <int> <id>]* => <int> <id>
  And returns a list with
  ( (name . num) (list (name . num) ...) )"
  (declare (string s))
  (let ((tokens (tokenize (string-right-trim '(#\Newline #\Return) s)))
        (reactants '())
        (product nil))
    (do ()
      ((null tokens)
       (list product (nreverse reactants)))
      (cond ((string= "=>" (car tokens))
             (pop tokens)
             (setq product
                   (parse-rule (pop tokens) (pop tokens))))
            (t
              (push (parse-rule (pop tokens) (pop tokens))
                    reactants))))))

(defun link-reactions (reactions reactants)
  "Fix up the reactions to replace reactant symbols with their indicies"
  (dolist (reaction reactions)
    (dolist (step (cadr reaction))
      (setf (rule-reactant step)
            (position (rule-reactant step) reactants))))
  (values (coerce reactions 'vector)
          (coerce reactants 'vector)))

(defun load-reactions (filename)
  "Create a vector of reactions that maps
  reactions[PRODUCT] = (quantity . (list reactants))"
  (let ((reactions '())
        (reactants '()))
    (with-open-file (s (pathname filename))
      (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof)
         (link-reactions (cons 'nil reactions)
                         (cons 'ore reactants)))
        (let* ((reaction (parse-reaction l))
               (inputs   (cdr reaction))
               (product  (car reaction))
               (reactant (rule-reactant product))
               (produced (rule-quantity product)))
          (push reactant reactants)
          (push (cons produced inputs) reactions))))))

;;; -------------------------------------------------------------------

(defstruct crucible
  "A vessel that holds amounts of different reactants.
  Tracks both current quantities and total amount added."
  (reactants  (make-array 0) :type simple-vector)
  (quantities (make-array 0) :type simple-vector)
  (totals     (make-array 0) :type simple-vector))

(defun new-crucible (reactants)
  "Create a new crucible that can hold size elements"
  (let ((size (length reactants)))
    (make-crucible
      :reactants  (make-array size :initial-contents reactants)
      :quantities (make-array size :initial-element 0)
      :totals     (make-array size :initial-element 0))))

(defun reactant-index (reactant crucible)
  "The index of this reactant."
  (declare (symbol reactant) (crucible crucible))
  (position reactant (crucible-reactants crucible)))

;; Simple value accessors
(declaim (ftype (function ((or symbol fixnum) crucible) integer)
                crucible-total crucible-quantity crucible-consumed))

(defun crucible-total (reactant crucible)
  "Find the total amount of reactant that has passed through the crucible"
  (typecase reactant
    (symbol (crucible-total (reactant-index reactant crucible) crucible))
    (fixnum (svref (crucible-totals crucible) reactant))))

(defun crucible-quantity (reactant crucible)
  "Find the current amount of reactant in the crucible"
  (typecase reactant
    (symbol (crucible-quantity (reactant-index reactant crucible) crucible))
    (fixnum (svref (crucible-quantities crucible) reactant))))

(defun crucible-consumed (reactant crucible)
  "How much of this reactant was consumed so far?"
  (typecase reactant
    (symbol (crucible-consumed (reactant-index reactant crucible) crucible))
    (fixnum (- (svref (crucible-totals     crucible) reactant)
               (svref (crucible-quantities crucible) reactant)))))

;; Operational mutators

(declaim (ftype (function ((or symbol fixnum) fixnum fixnum crucible) null)
                set-crucible-contents))

(defun set-crucible-contents (reactant totals contents crucible)
  "Initialize the crucible's record of this type of material. Returns nil"
  (typecase reactant
    (symbol (set-crucible-contents (reactant-index reactant crucible) totals contents crucible))
    (fixnum (setf (svref (crucible-totals     crucible) reactant) totals)
            (setf (svref (crucible-quantities crucible) reactant) contents)
            nil)))

(declaim (ftype (function ((or symbol fixnum) fixnum crucible) fixnum)
                add-to-crucible))

(defun add-to-crucible (reactant quantity crucible)
  "Add material to the crucible. Returns the new level of reactant in the crucible." 
  (typecase reactant
    (symbol (add-to-crucible (reactant-index reactant crucible) quantity crucible))
    (fixnum (incf (svref (crucible-totals     crucible) reactant) quantity)
            (incf (svref (crucible-quantities crucible) reactant) quantity))))

(declaim (ftype (function ((or symbol fixnum) fixnum crucible) (or null fixnum))
                consume-from-crucible))

(defun consume-from-crucible (reactant quantity crucible)
  "Consume a quantity of reactant from the crucible.
  Returns new reactant amount or NIL if unable to comply"
  (typecase reactant
    (symbol (consume-from-crucible (reactant-index reactant crucible) quantity crucible))
    (fixnum (when (>= (svref (crucible-quantities crucible) reactant) quantity)
              (decf (svref (crucible-quantities crucible) reactant) quantity)))))

(defun crucible-empty-p (crucible)
  "T if there are no reactants remaining in the crucible.
  Note that 'ore (always index zero) does not count as a reactant"
  (not (find-if-not #'zerop (crucible-quantities crucible) :start 1)))

;;; -------------------------------------------------------------------

(defparameter *infinite-product* nil
  "A list of products that have an infinite supply")

(defun produce (goal required rules crucible)
  "Produce the goal from elements in the crucible.
  Returns T if able to produce, or NIL otherwise"
  (declare (fixnum goal) (integer required) (crucible crucible))

  (labels ((goal-quantity () (crucible-quantity goal crucible)))
    (cond 
      ;; If we have enough, we don't need to produce any more
      ((>= (goal-quantity) required))

      ;; If we have infinite ore, we can just mine some more
      ((member goal *infinite-product*)
       ;; Add just enough to satisfy this requirement.
       (add-to-crucible goal (- required (goal-quantity)) crucible)
       t)

      ;; If we found some production rules, we can produce what we need.
      ((not (zerop goal))
       (let* ((formula           (svref rules goal))
              (produced-quantity (car formula))
              (formula-rules     (cadr formula)))

         ;; Run production until we meet our requirements
         (do () ((>= (goal-quantity) required) t)

           ;; Run the rules, if any
           (dolist (rule formula-rules)
             (let ((reactant (rule-reactant rule))
                   (quantity (rule-quantity rule)))
               (if (produce reactant quantity rules crucible)
                 (consume-from-crucible reactant quantity crucible)
                 (return-from produce nil))))

           ;; Provide the rule's level of resources
           (add-to-crucible goal produced-quantity crucible))))

      ;; We don't have enough and we can't make more, so return nil
      (t nil))))

;;; -------------------------------------------------------------------

(defun run (filename)
  (declare ((or string pathname) filename))
  (multiple-value-bind (reactions reactants) (load-reactions filename)
    (let* ((crucible (new-crucible reactants))
           (*infinite-product* '(ore)))

      ;; Produce one unit of fuel
      (produce (reactant-index 'fuel crucible) 1 reactions crucible)

      ;; Report fuel generated and ore consumed
      (format t "Ore consumed = ~A~%" (crucible-consumed 0 crucible))
      (format t "Fuel produced = ~A~%" (crucible-quantity 'fuel crucible))

      ;; The result is our crucible full of products and byproducts
      crucible)))

;;; -------------------------------------------------------------------

(defun do-consume (ore goal reactions crucible)
  "How much fuel can you make from the given parameter?"
  (declare (integer ore) (crucible crucible))

  ;; force crucible 'ore levels to the specified amount
  (set-crucible-contents 'ore ore ore crucible)

  ;; Produce our fuel
  (let ((used-fuel 0))
    (loop
      (unless (produce goal 1 reactions crucible)
        (return used-fuel))

      ;; Consume our fuel
      (consume-from-crucible goal 1 crucible)
      (incf used-fuel)

      (when (crucible-empty-p crucible)
        ;; We have returned to a steady state
        ;; Compute how many times we can do this again with the amount
        ;; of ore we have remaining
        (multiple-value-bind
          (runs leftovers) (floor ore (crucible-consumed 0 crucible))
          (let* ((runs-fuel (* runs used-fuel))
                 (total-fuel
                   (if (zerop leftovers)
                     runs-fuel
                     (+ runs-fuel
                        (do-consume leftovers goal reactions crucible)))))
            (return-from do-consume total-fuel)))))))


(defun consume (filename ore)
  (multiple-value-bind (reactions reactants) (load-reactions filename)
    (let ((crucible (new-crucible reactants))
          (*infinite-product* (if ore '() (list 'ore))))
      (do-consume ore (reactant-index 'fuel crucible) reactions crucible))))

;;; -------------------------------------------------------------------

(defvar *db*  #P"day_14.txt")
(defvar *db1* #P"day_14.1.txt")
(defvar *db2* #P"day_14.2.txt")
(defvar *db3* #P"day_14.3.txt")
(defvar *db4* #P"day_14.4.txt")
