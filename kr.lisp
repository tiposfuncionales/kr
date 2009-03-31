;;; -*- Mode: LISP; package: KR; Editor: T -*-


;;; ______________________________________________________________________
;;;
;;; This code was written as part of the User Interface (Dante) project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or anything developed as part of the Dante
;;; Project, please contact Dario Giuse (dzg@spice.cs.cmu.edu).
;;; ______________________________________________________________________
;;;




;;; This file implements KR, a kernel knowledge representation system.  Unlike
;;; other systems, KR implements a small, carefully selected amount of
;;; functionality in order to keep performance within reasonable limits.
;;; 
;;; KR was loosely inspired by SRL and by CRL.
;;; 
;;; SRL was developed by Mark Fox for the Intelligent Systems Laboratory of
;;; Carnegie Mellon University, Pittsburgh, PA.
;;; 
;;; CRL is a registered trade mark of Carnegie Group Inc., Pittsburgh, PA.
;;; 

;;; Let's be smart about CLtL2 compatible Lisps:
(eval-when (compile load eval)
  #+(or (and :excl (or :allegro-v4.0 :allegro-v4.1))
	:mcl
	(and :lucid :lcl4.0)
	(and :cmu :new-compiler)
	:clisp
	)
  (pushnew :cltl2 *features*))


#-:cltl2
(in-package "KR" :use)

#+:cltl2
(defpackage "KR" 
  (:shadow defclass make-instance defmethod call-next-method)
  (:use #-:lucid "COMMON-LISP"
	#+:lucid "LISP" #+:lucid "LUCID-COMMON-LISP"))
#+:cltl2
(unless (find-package "KR") 
  (make-package  "KR")
  (shadow '(defclass make-instance defmethod call-next-method) "KR")
  (use-package '("COMMON-LISP") "KR"))
#+:cltl2
(in-package "KR")

(pushnew :framework *features*)


;(eval-when (compile load eval)
  ;; Make packages...
;  (if (not (find-package "KR"))
;      (make-package "KR" :use '("LISP"))))



(export '(PS
	  CREATE-SCHEMA CREATE-FRESH-SCHEMA COPY-SCHEMA DELETE-SCHEMA SCHEMA-P
	  CREATE-RELATION RELATION-P IS-A-P
	  CREATE-SLOT DELETE-SLOT HAS-SLOT-P DO-SLOTS
	  GET-SLOTS GET-ALL-SLOTS
	  GET-VALUE GET-VALUES GET-LOCAL-VALUES DOVALUES DO-ALL-VALUES
	  SCHEMA-CALL
	  SET-VALUE SET-VALUES SET-VALUE-N APPEND-VALUE DELETE-VALUE-N))



;;; 
;;; A schema is represented in KR as the P-LIST of a symbol.  The symbol itself
;;; is the schema name.
;;; 
;;; Each slot is represented as an entry in the P-LIST.  The value of the slot is
;;; always a list, or nil if the slot does not contain any value.  It is
;;; conventional to use keywords as both schema names and slot names.
;;; 





;;; 
(defvar *inheritance-relations* '()
  "All relations in this list perform inheritance.")


(defvar *relations* '()
  "An a-list of relations known to the system, with their inverse(s).
   Used for the creation of automatic reverse-links.")



;;; --------------------------------------------------


(defun schema-p (name)
  "Returns T if <name> is the name of a valid schema."
  (and (symbolp name)
       (symbol-plist name)))

(defun is-a-p (schema type)
  "Test whether <schema> IS-A <type>"
  (let ((parent-list (get schema :IS-A)))
    (when parent-list
      (if (member type (the list parent-list))
	  t
	  ;; Not directly in the list: how about the parents?
	  (dolist (parent parent-list)
	    (if (is-a-p parent type)
		(return-from is-a-p t)))))))

(defmacro relation-p (slot)
  `(member :relation (get ,slot :is-a)))


(defun create-relation (relation inheritance-p inverses)
  "Defines a new relation with its inverses.  In <inheritance-p> is non-nil, classifies the relation as one that performs inheritance."
  (setf (get relation :is-a) '(:relation)
	(get relation :inverse) inverses)
  (when inheritance-p
    ;; Add to the end of the list.
    (setf *inheritance-relations*
	  (append *inheritance-relations* (list relation))))
  (push (cons relation inverses) *relations*)
  (dolist (inv inverses)
    (let ((entry (assoc inv *relations*)))
      (if entry
          (pushnew relation (cdr entry))
	  (progn
	   (push (list inv relation) *relations*)
	   (setf (get inv :is-a) '(:relation)
		 (get inv :inverse) (list relation)))))))





(defun unlink-one-value (schema slot value)
  "Remove the inverse link from <value> to <schema>, following the inverse of <slot>."
  (let ((inverse (first (cdr (assoc slot *relations*)))))
    (when inverse
      ;; If the terminal has an INVERSE slot, remove <schema> from the
      ;; inverse slot.
      (setf (get value inverse)
	    (delete schema (get value inverse))))))

(defun unlink-all-values (schema slot)
  "Same as unlink-one-value, but unlinks all schemata that are in <slot>."
  (let ((inverse (first (cdr (assoc slot *relations*)))))
    (when inverse
      (dolist (value (get schema slot))
	;; If the terminal has an INVERSE slot, remove <schema> from the
	;; inverse slot.
	(setf (get value inverse)
	      (delete schema (get value inverse)))))))




 
(defun link-in-relation (schema slot values)
  "Since <value> got added to <slot>, see if we need to put in an inverse link to <schema> from <value>. This happens when <slot> is a relation with an inverse."
  (when (schema-p slot)
    (let ((inverse (first (cdr (assoc slot *relations*)))))
      (when inverse
	;; Does the terminal already have an INVERSE slot ?
	(if (listp values)
	    ;; <values> is a list: cycle through them all
	    (dolist (value values)
	      (let ((target-slots (get value inverse)))
		;; Create the back-link.  We use primitives to avoid looping.
		(if target-slots
		    (pushnew schema (get value inverse))
		    (setf (get value inverse) (list schema)))))
	    (let ((target-slots (get values inverse)))
	      ;; Create the back-link.  We use primitives here to avoid looping.
	      (if target-slots
		  (pushnew schema (get values inverse))
		  (setf (get values inverse) (list schema)))))))))
    
(defun replace-old-values (schema slot old-values new-value)
  "Destroy a link and create a new one, if necessary."
  (let ((inverse (first (cdr (assoc slot *relations*))))
	(was-present (find new-value old-values)))
    (when inverse
      (dolist (old-value old-values)
	(unless was-present
	  ;; If <old-value> was a symbol, we will destroy the inverse link.
	  (when (symbolp old-value)
	    ;; If the terminal has an INVERSE slot, remove <schema> from the
	    ;; inverse slot.
	    (setf (get old-value inverse)
		  (delete schema (get old-value inverse))))))
      ;; Similarly, if <new-value> is a symbol, we create a new inverse link.
      (when (and (symbolp new-value) (not was-present))
	(link-in-relation schema slot new-value)))))


(defun copy-schema (schema)
  "Returns an identical copy of a schema."
  (when (symbolp schema)
    (let ((new-schema (gensym)))
      (setf (symbol-plist new-schema) (copy-tree (symbol-plist schema)))
      new-schema)))

(defun delete-schema (schema)
  "Deletes the <schema>."
  ;; Remove all inverse links.
  (do ((slot (symbol-plist schema) (cddr slot)))
      ((null slot))
    (when (relation-p (first slot))
      (unlink-all-values schema (first slot))))
  ;; Eliminate all the slots.
  (setf (symbol-plist schema) nil)
  ;; If this was a relation, eliminate it too.
  (let ((entry (assoc schema *relations*)))
    (when entry
      (setf *relations* (delete entry *relations*))
      ;; remove any INVERSE links.
      (dolist (relation *relations*)
	(if (find schema (cdr relation))
	    (setf (cdr relation) (delete schema (cdr relation)))))
      (when (find schema *inheritance-relations*)
	(setf *inheritance-relations* (delete schema *inheritance-relations*))))))

(defun create-slot (schema slot)
  (unless (get schema slot)
    (setf (get schema slot) nil)))

(defun delete-slot (schema slot)
  (remprop schema slot))

(defun has-slot-p (schema slot)
  (if (member slot (symbol-plist schema)) t))

(defun get-slots (schema)
  "Returns the names of all the local slots in the <schema>, as a list."
  (do ((names nil)
       (slot (symbol-plist schema) (cddr slot)))
      ((null slot)
       (nreverse names))
    (push (car slot) names)))


 
(defun do-slots (schema function)
  "The <function>, a function of two arguments, is applied to each slot of the
   <schema> in turn.  The <function> is called with the schema as its first
   argument and the slot name as its second argument."
  (do ((slot (symbol-plist schema) (cddr slot)))
      ((null slot))
    (funcall function schema (car slot))))

(defun internal-all-slots (schema slots)
  (do ((slot (symbol-plist schema) (cddr slot)))
      ((null slot)
       slots)
    (pushnew (car slot) slots)
    (when (member (car slot) *inheritance-relations*)
      (dolist (value (cadr slot))
	(when (symbolp value)
	  (setf slots (internal-all-slots value slots)))))))

(defmacro get-all-slots (schema)
  `(internal-all-slots ,schema nil))

(defun get-values-internal (schema slot)
  (do ((p (symbol-plist schema) (cddr p)))
      ((null p))
    (when (member (car p) *inheritance-relations*)
      (dolist (parent (cadr p))
	(when parent
	  (let ((values (or (get parent slot)
			    (get-values-internal parent slot))))
	    (if values (return-from get-values-internal values))))))))

(defmacro get-value (schema slot)
  `(or (car (get ,schema ,slot))
       ;; Slot not found - use inheritance
       (car (get-values-internal ,schema ,slot))))


#|
;;; Version for demons.
;;; 
(defun get-value (schema slot)
  (or (car (get schema slot))
      ;; Slot not found - use inheritance
      (car (get-values-internal schema slot))))
|#



;;;; GET-VALUES
;;; 
(defmacro get-values (schema slot)
  `(or (get ,schema ,slot)
       ;; Slot not found - use inheritance
       (get-values-internal ,schema ,slot)))



;;;; GET-LOCAL-VALUES
;;; 
(defmacro get-local-values (schema slot)
  `(get ,schema ,slot))



;;; ----------------------------------------------------------------



;;; All functional entries have an argument list whose first argument is the
;;; schema in which it was found
;;; 
(defmacro schema-call (schema field &rest args)
  `(let ((sc ,schema))
     (funcall (get-value sc ,field) sc ,@args)))



;;; Just as before, but skips the local values
;;; 
(defmacro schema-call-inherited (schema field &rest args)
  `(let ((sc ,schema))
     (funcall (car (get-values-internal sc ,field)) sc ,@args)))




;;;; DOVALUES
;;; Executes <body> with <var> bound to all the values of <slot> in <schema>.  Note
;;; that the values are as per get-values.
;;; 
(defmacro dovalues ((var schema slot) &rest body)
  `(let ((values (get-values ,schema ,slot)))
     (if values
	 (dolist (,var values)
	   ,@body))))



(defun map-all-values (function schema slot)
  "Maps <function> to all the values in <slot> of <schema>."
  (let ((vals (get schema slot)))
    (if vals
	(map nil function vals)
	(dolist (parent (get-values schema :is-a))
	  (map-all-values function parent slot)))))



;;;; DO-ALL-VALUES
;;; Executes <body> with <var> bound to all the values of <slot> in <schema>.
;;; Proceeds up all the :is-a links, not just the first one that finds any
;;; value.  The difference is important when <schema> :is-a more than one
;;; thing.
;;; 
(defmacro do-all-values ((var schema slot) &rest body)
  `(map-all-values #'(lambda (v)
		       (let ((,var v))
			 ,@body))
		   ,schema
		   ,slot))

(defun set-value (schema slot value)
  "Replaces the contents of <slot> in the schema <name> with the single <value>."
  (let ((entry (get schema slot)))
    ;; Is the slot a relation? If so, deal with its inverse.
    (when (relation-p slot)
      (replace-old-values schema slot entry value))
    (cond ((car entry)
	   (if (cdr entry) (setf (cdr entry) nil))
	   (setf (car entry) value))
	  (t
	   (setf (get schema slot) (list value))
	   value))))

(defun set-values (schema slot values)
  "Replaces the contents of <slot> in the schema <name> with the <values>."
  (unless (get schema slot)
    (setf (get schema slot) nil))
  (do ((p (symbol-plist schema) (cddr p)))
      ((null p))
    (when (eq (car p) slot)
      ;; Is the slot a relation? If so, we may have to deal with the inverse.
      (when (relation-p slot)
	;; Unlink all previous values
	(unlink-all-values schema slot)
	(link-in-relation schema slot values))
      ;; Set the slot, return the values.
      (return (setf (cadr p) values)))))



;;; Define a setf form for both GET-VALUE and GET-VALUES
;;; 

(defsetf get-value set-value)

(defsetf get-values set-values)




;;;; SET-VALUE-N
;;; Inputs:
;;; - <name>: the name of a schema
;;; - <position>: a 0-based integer
;;; 
(defun set-value-n (schema slot value position)
  (let ((entry (get schema slot)))
    ;; Do nothing if the position is past the end of the list of values.
    (when (< -1 position (length entry))
      ;; Is the slot a relation? If so, we may have to deal with the inverse.
      (when (relation-p slot)
	(unlink-one-value schema slot (nth position entry))
	(link-in-relation schema slot value))
      (setf (nth position entry) value))))


(defun append-value (schema slot value)
  (when (relation-p slot)
    (link-in-relation schema slot value))
  (let ((values (get schema slot)))
    (if values
	(setf (cdr (last values)) (list value))
	(setf (get schema slot) (list value)))))

(defun delete-value-n (schema slot position)
  (let ((entry (get schema slot)))
    (when entry
      ;; Do nothing if the slot is not there
      (when (< -1 position (length entry))
	;; Eliminate the position-th element from the list of values.
	(if (= position 0)
	    (pop (get schema slot))
	    (pop (cdr (nthcdr (1- position) (get schema slot)))))
	t))))


(defun ps (schema)
  (if schema
      (let ((slots (symbol-plist schema)))
	(format t "{{~A~%" schema)
	(do ((slot slots (cddr slot)))
	    ((null slot))
	  (format t "  ~A: ~:[~S~;~{~S ~}~]~%"
		  (first slot) (listp (second slot)) (second slot)))
	(format t "  }}~%"))
      t))




;;; --------------------------------------------------


;;; Auxiliary function for all the create-schema family of functions.
;;; 
(defun create-schema-body (schema init rest)
  (when rest
    (do ((tail rest (cdr tail))
	 (body nil))
	((null tail)
	 ; If it's an instance, initialise it.
	 (nreverse
	  (cons `(if (and (get-local-values ,schema :instance)
			  (setf ,init
				(car (get-values-internal ,schema :init-hook))))
		     (funcall init schema))
		body)))
      (let ((slot-init (car tail)))
	(when (consp slot-init)
	  (let ((slot-name (car slot-init))
		(slot-val (cdr slot-init)))
	    (when slot-val
	      (if (consp (cdr slot-val))
		  (push `(set-values ,schema ,slot-name '(,@slot-val)) body)
		  (push `(set-value ,schema ,slot-name ,@slot-val) body)))))))))


;;;; CREATE-SCHEMA
;;; 
(defmacro create-schema (name &rest rest)
  `(let ((schema ,name)
	 ,@(when rest '(init)))
     ,@(create-schema-body 'schema 'init rest)
     schema))


;;;; CREATE-FRESH-SCHEMA
;;; like create-schema, but wipes out old schema first.
;;; 
(defmacro create-fresh-schema (name &rest rest)
  `(let ((schema ,name)
	 ,@(when rest '(init)))
     (delete-schema schema)
     ,@(create-schema-body 'schema 'init rest)
     schema))




(defun initialize-kr ()
  (setf *inheritance-relations* nil)
  ;; The is-a relation, which should come first in the list.
  (create-relation :IS-A T '(:is-a-inv))
  ;; Now for the instance relation (temporarily NON-INHERITING)
  (create-relation :INSTANCE nil '(:instance-inv)))


(initialize-kr)


;;; -------------------------------------------------- Demons







;;; --------------------------------------------------
;;;
;;; Proclaim that the system was loaded successfully
;;;
(setf (get :dante-modules :KR) t)
