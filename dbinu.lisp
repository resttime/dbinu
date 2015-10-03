(in-package :dbinu)

(defvar *ts* (make-hash-table))
(defvar *spo* (make-hash-table :test 'equal))
(defvar *pos* (make-hash-table :test 'equal))
(defvar *osp* (make-hash-table :test 'equal))

(defstruct (triple (:conc-name nil))
  subject
  predicate
  object
  graph
  uuid)

(defencoding triple
  subject predicate object graph id)

(defun all-uuids ()
  "Returns a list of all uuids of the triples in the triplestore."
  (loop for uuid being the hash-keys of *ts*
     collect uuid))

(defun all-triples ()
  "Returns a list of all triples in the triplestore."
  (loop for triple being the hash-values of *ts*
     collect triple))

(defun all-subjects ()
  (loop for subject being the hash-keys of *spo*
     collect subject))

(defun all-predicates ()
  (loop for predicate being the hash-keys of *pos*
     collect predicate))

(defun all-objects ()
  (loop for objects being the hash-keys of *osp*
     collect objects))

(defun add-entry (index entry)
  "Index a single entry."
  (cond ((eq (cddr entry) nil) (pushnew (cadr entry) (gethash (car entry) index) :test 'equal))
	(t (add-entry  (if (eq (gethash (car entry) index) nil)
			   (setf (gethash (car entry) index) (make-hash-table :test 'equal))
			   (gethash (car entry) index))
		       (cdr entry)))))

(defun add-entries (index entries)
  "Index every entry in entries."
  (mapc (lambda (entry) (add-entry index entry)) entries))

(defun add-to-indices (triple)
  "Index the triple."
  (with-slots (subject predicate object) triple
    (add-entry *spo* (list subject predicate object))
    (add-entry *pos* (list predicate object subject))
    (add-entry *osp* (list object subject predicate))))

(defun rebuild-indicies ()
  "Literally tries to rebuild every index."
  (mapc #'add-to-indices (all-triples)))

(defun add-triple (entry)
  "Create a single triple from the entry and indexes it as well."
  (let* ((triple-id (intern (print-bytes nil (make-v1-uuid))))
	 (new-triple (make-triple :subject (first entry) :predicate (second entry) :object (third entry)
				  :graph (cadddr entry) :uuid triple-id)))
    (setf (gethash triple-id *ts*) new-triple)
    (add-to-indices new-triple)))

(defun add-triples (entries)
  "For every entry, create a triple and add it to the *ts* with a UUID key.
Supplying a graph in the entry is optional.
Entry: (subject predicate object graph)"
  (mapc #'add-triple entries))

(defun filter-triples (triples &optional parameters)
  "Returns a list with all triples that match the parameters.
Try to order the queries in an ascending order regarding the number of likely matches.
Usage: (filter-triples (all-triples *my-triplestore*) '((predicate . has-tag) (object . yo)))"
  (cond ((eq parameters nil) triples)
	(t (filter-triples (remove-if-not (lambda (triple) (equal (slot-value triple (caar parameters))
								  (cdar parameters)))
					  triples)
			   (cdr parameters)))))

(defun remove-triples (parameters)
  "Remove all triples in the triplestore that match the parameters.
Parameters: '((subject NSA) (object evidence))"
  (let ((uuids (loop for triple in (filter-triples (all-triples) parameters)
		       collect (uuid triple))))
    (mapc (lambda (uuid) (remhash uuid *ts*)) uuids)))

(defun save-ts (filepath)
  (with-open-file (out filepath
		       :direction :output
		       :if-exists :supersede
		       :element-type '(unsigned-byte 8))
    (write-sequence (encode *ts*) out)))

(defun load-ts (filepath)
  (setf *ts* (decode-file filepath))
  (rebuild-indicies))
