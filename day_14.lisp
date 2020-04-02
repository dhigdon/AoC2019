;;;; Day 14 - chemical reactions
(require 'cl-utilities)
(use-package 'cl-utilities)

;;; -------------------------------------------------------------------

;;; Data conversion - parse the rules

(defstruct rule
  (reactant nil :type symbol)
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

(defun load-reactions (filename)
  "Create a hashtable of reactions that maps
  PRODUCT -> (quantity . (list reactants))"
  (let ((reactions (make-hash-table))
        (reactants '()))
    (with-open-file (s (pathname filename))
      (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof)
         (values reactions reactants))
        (let* ((reaction (parse-reaction l))
               (inputs   (cdr reaction))
               (product  (car reaction))
               (reactant (rule-reactant product))
               (produced (rule-quantity product)))
          (push reactant reactants)
          (setf (gethash reactant reactions) (cons produced inputs)))))))

(defun show-reactions (table)
  (loop for x being the hash-values of table using (hash-key k)
        collect (list (cons k (car x)) (cdr x))))

;;; -------------------------------------------------------------------

(defstruct crucible
  "A vessel that holds amounts of different reactants.
  Tracks both current quantities and total amount added."
  (quantities (make-hash-table) :type hash-table)
  (totals     (make-hash-table) :type hash-table))

(defun new-crucible (size)
  "Create a new crucible that can hold size elements"
  (make-crucible
    :quantities (make-hash-table :size size)
    :totals     (make-hash-table :size size)))

(declaim (ftype (function (symbol crucible) integer) crucible-total crucible-quantity))

;; The crucible maps reactants to a record of (produced . consumed)
(defun crucible-total (reactant crucible)
  "Find the total amount of reactant that has passed through the crucible"
  (gethash reactant (crucible-totals crucible) 0))

(defun crucible-quantity (reactant crucible)
  "Find the current amount of reactant in the crucible"
  (gethash reactant (crucible-quantities crucible) 0))

(defun set-crucible-contents (reactant totals contents crucible)
  "Initialize the crucible's record of this type of material"
  (setf (gethash reactant (crucible-totals crucible)) totals)
  (setf (gethash reactant (crucible-quantities crucible)) contents))

(defun add-to-crucible (reactant quantity crucible)
  "Add material to the crucible. Returns the new level of reactant in the crucible." 
  ;; Note, can't use incf because there may not yet be an entry for this reactant
  (let ((ht (crucible-totals crucible)))
    (setf (gethash reactant ht) (+ quantity (gethash reactant ht 0))))

  (let ((ht (crucible-quantities crucible)))
    (setf (gethash reactant ht) (+ quantity (gethash reactant ht 0)))))

(defun consume-from-crucible (reactant quantity crucible)
  "Consume a quantity of reactant from the crucible.
  Returns new reactant amount or NIL if unable to comply"
  (when (>= (crucible-quantity reactant crucible))
    (decf (gethash reactant (crucible-quantities crucible)) quantity)))

;;; -------------------------------------------------------------------

(defparameter *infinite-ore* t)

(defun produce (goal required rules crucible)
  "Produce the goal from elements in the crucible.
  Returns T if able to produce, or NIL otherwise"
  (declare (symbol goal) (integer required) (hash-table rules) (crucible crucible))

  (multiple-value-bind (formula found) (gethash goal rules)

    (cond 
      ;; If we have enough, we are done
      ((>= (crucible-quantity goal crucible) required))

      ;; If we have infinite ore, we can just mine some more
      ((and (eq goal 'ore) *infinite-ore*)
       (let ((deficit (- required (crucible-quantity goal crucible))))
         (add-to-crucible goal deficit crucible)
         t))

      ;; If we found some production rules, we can produce what we need.
      (found
        (let ((produced-quantity (car formula))
              (formula-rules     (cadr formula)))

          ;; Run production until we meet our requirements
          (do () ((>= (crucible-quantity goal crucible) required) t)

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

(defun run (filename &key  ore)
  (multiple-value-bind (reactions reactants) (load-reactions filename)
    (let ((crucible (new-crucible (length reactants)))
          (*infinite-ore* (not (numberp ore))))

      ;; If specified, then load our initial ore into the crucible
      (when (numberp ore)
        (set-crucible-contents 'ore ore ore crucible))

      ;; Produce our fuel
      (unless (produce 'fuel 1 reactions crucible)
        (error "Out of ore"))

      ;; Report fuel generated and ore consumed
      (when *infinite-ore*
        (format t "Ore consumed = ~A~%" (crucible-total 'ore crucible)))
      (format t "Fuel produced = ~A~%" (crucible-quantity 'fuel crucible))

      ;; The result is our crucible full of products and byproducts
      crucible)))

;;; -------------------------------------------------------------------

(defun crucible-empty (crucible exceptions)
  (maphash #'(lambda (k v)
               (unless (or (zerop v) (member k exceptions))
                 (return-from crucible-empty nil)))
           (crucible-quantities crucible))
  t)


(defun do-consume (ore reactions crucible)
  "How much fuel can you make from the given parameter?"
  (declare (integer ore) (hash-table reactions) (crucible crucible))

  (set-crucible-contents 'ore ore ore crucible)

  ;; Produce our fuel
  (let ((total-fuel 0))
    (loop
      (unless (produce 'fuel 1 reactions crucible)
        (return total-fuel))

      ;; Consume our fuel
      (consume-from-crucible 'fuel 1 crucible)
      (incf total-fuel)

      (when (crucible-empty crucible '(ore))
        ;; We have returns to a steady state
        ;; Compute how many times we can do this again with the amount
        ;; of ore we have remaining
        (let ((used-ore (- ore (crucible-quantity 'ore crucible))))
          (multiple-value-bind (runs leftovers) (floor ore used-ore)
            (return-from do-consume
                         (if (zerop leftovers)
                           (* runs total-fuel)
                           (+ (* runs total-fuel)
                              (do-consume leftovers reactions crucible))))))))))


(defun consume (filename ore)
  (multiple-value-bind (reactions reactants) (load-reactions filename)
    (let ((crucible (new-crucible (length reactants)))
          (*infinite-ore* (null ore)))
      (do-consume ore reactions crucible))))

;;; -------------------------------------------------------------------

(defparameter *db* #P"day_14.txt")
(defparameter *db1* #P"day_14.1.txt")
(defparameter *db2* #P"day_14.2.txt")
(defparameter *db3* #P"day_14.3.txt")
(defparameter *db4* #P"day_14.4.txt")
