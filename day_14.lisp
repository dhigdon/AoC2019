;;;; Day 14 - chemical reactions
(require 'cl-utilities)
(use-package 'cl-utilities)

;;; -------------------------------------------------------------------

;;; Data conversion - parse the rules

(defun tokenize (s)
  (split-sequence #\Space (remove #\, s)))

(defun parse-rule (num name)
  (cons (intern name) (parse-integer num)))

(defun make-reactant (r q) (cons r q))
(defun rule-reactant (r) (car r))
(defun rule-quantity (r) (cdr r))

;;; -------------------------------------------------------------------

(defun parse-reaction (s)
  "Takes an input string of the form
  <int> <id>[, <int> <id>]* => <int> <id>
  And returns a list with
  ( (name . num) (list (name . num) ...) )"
  (let ((tokens (tokenize (string-right-trim '(#\Newline #\Return) s)))
        (reactants '())
        (product nil))
    (do ()
      ((null tokens)
       (list product (reverse reactants)))
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
               (reactant (car product))
               (produced (cdr product)))
          (push reactant reactants)
          (setf (gethash reactant reactions)
                (cons produced inputs)))))))

(defun show-reactions (table)
  (loop for x being the hash-values of table using (hash-key k)
        collect (list (cons k (car x)) (cdr x))))

;;; -------------------------------------------------------------------

(defstruct product
  (total   0 :type integer)
  (current 0 :type integer))

(declaim (ftype (function (hash-table symbol) integer) crucible-quantity crucible-produced)
         (ftype (function (hash-table symbol integer) product) add-crucible-quantity)
         (ftype (function (hash-table symbol integer) (or integer null)) use-crucible-quantity))

;; The crucible maps reactants to a record of (produced . consumed)
(defun crucible-produced (crucible product)
  "Find the total amount of product that has passed through the crucible"
  (multiple-value-bind (c found) (gethash product crucible)
    (if found (product-total c) 0)))

(defun crucible-quantity (crucible product)
  "Find the current amount of product in the crucible"
  (multiple-value-bind (c found) (gethash product crucible)
    (if found (product-current c) 0)))

(defun add-crucible-quantity (crucible product q)
  "Add material to the crucible. Returns the new product record"
  (multiple-value-bind (c found) (gethash product crucible)
    (cond (found
            (incf (product-total c) q)
            (incf (product-current c) q)
            c)
          (t (setf (gethash product crucible) (make-product :total q :current q))))))

(defun use-crucible-quantity (crucible product q)
  "Use q units of product. Returns NIL if unable to comply"
  (multiple-value-bind (c found) (gethash product crucible)
    (when (and found (>= (product-current c) q))
      (decf (product-current c) q))))

;;; -------------------------------------------------------------------

(defparameter *infinite-ore* t)

(defun produce (goal required rules crucible)
  "Produce the goal from elements in the crucible.
  Returns T if able to produce, or NIL otherwise"
  (declare (symbol goal) (integer required) (hash-table rules crucible))

  (multiple-value-bind (formula found) (gethash goal rules)

    (cond 
      ;; If we have enough, we are done
      ((>= (crucible-quantity crucible goal) required))

      ;; If we have infinite ore, we can just mine some more
      ((and (eq goal 'ore) *infinite-ore*)
       (let ((deficit (- required (crucible-quantity crucible goal))))
         ;(format t "Mining for ~A more ore~%" deficit)
         (add-crucible-quantity crucible goal deficit)
         t))

      ;; If we found some production rules, we can produce what we need.
      (found
        ;(format t "Producing components to make ~A units of ~A...~%" required goal)
        (let ((produced-quantity (car formula))
              (production-rules  (cadr formula)))

          ;; Run production until we meet our requirements
          (do () ((>= (crucible-quantity crucible goal) required)
                  t)

            ;; Run the rules, if any
            (dolist (rule production-rules)
              (let ((reactant (rule-reactant rule))
                    (quantity (rule-quantity rule)))
                ;(format t "  ~A ~A -> ~A ~A~%" quantity reactant required goal)
                (if (produce reactant quantity rules crucible)
                  (use-crucible-quantity crucible reactant quantity)
                  (return-from produce nil))))

            ;; Provide the rule's level of resources
            ;(format t "Produced ~A units of ~A~%" produced-quantity goal)
            (add-crucible-quantity crucible goal produced-quantity))))

      ;; We don't have enough and we can't make more, so return nil
      (t nil))))

;;; -------------------------------------------------------------------

(defun run (filename &key  ore)
  (multiple-value-bind (reactions reactants) (load-reactions filename)
    (let ((crucible (make-hash-table :size (length reactants))))
      ;; Clear the crucible's contents
      ;; NOTE: crucible contains a pair of (produced . consumed)

      ;; We start off with one trillion units of ore
      (if (numberp ore)
        (setf (gethash 'ore crucible) (make-product :total ore :current ore)
              *infinite-ore* nil)
        (setf *infinite-ore* t))

      ;; Produce our fuel
      (unless (produce 'fuel 1 reactions crucible)
        (error "Out of ore"))

      ;; Report fuel generated and ore consumed
      (when *infinite-ore*
        (format t "Ore consumed = ~A~%" (gethash 'ore crucible)))
      (format t "Fuel produced = ~A~%" (gethash 'fuel crucible))

      ;; The result is our crucible full of products and byproducts
      crucible)))

;;; -------------------------------------------------------------------

(defun crucible-empty (crucible exceptions)
  (maphash #'(lambda (k v)
               (unless (or (zerop (product-current v))
                           (member k exceptions))
                 (return-from crucible-empty nil)))
           crucible)
  t)


(defun do-consume (ore reactions crucible)
  "How much fuel can you make from the given parameter?"
  (declare (integer ore) (hash-table reactions crucible))
  (setf (gethash 'ore crucible) (make-product :total ore :current ore))

  ;; Produce our fuel
  (let ((total-fuel 0))
    (loop
      (unless (produce 'fuel 1 reactions crucible)
        (return total-fuel))

      ;; Consume our fuel
      (use-crucible-quantity crucible 'fuel 1)
      (incf total-fuel)

      (when (crucible-empty crucible '(ore))
        ;; We have returns to a steady state
        ;; Compute how many times we can do this again with the amount
        ;; of ore we have remaining
        ;(format t "Found a cycle ~A fuel units long~%" total-fuel)
        (let ((used-ore (- ore (crucible-quantity crucible 'ore))))
          (multiple-value-bind (runs leftovers) (floor ore used-ore)
            ;(format t "~A ore used, ~A runs, ~A leftovers~%" used-ore runs leftovers)
            (return-from
              do-consume
              (if (zerop leftovers)
                (* runs total-fuel)
                (+ (* runs total-fuel)
                   (do-consume leftovers reactions crucible))))))))))


(defun consume (filename ore)
  (multiple-value-bind (reactions reactants) (load-reactions filename)
    (let ((crucible (make-hash-table :size (length reactants)))
          (*infinite-ore* (null ore)))
      (do-consume ore reactions crucible))))

;;; -------------------------------------------------------------------

(defparameter *db* #P"day_14.txt")
(defparameter *db1* #P"day_14.1.txt")
(defparameter *db2* #P"day_14.2.txt")
(defparameter *db3* #P"day_14.3.txt")
(defparameter *db4* #P"day_14.4.txt")
