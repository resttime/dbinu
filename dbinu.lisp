(in-package #:dbinu)

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

(defun collect-hash-keys (ht)
 (loop for key being the hash-keys of ht
    using (hash-value value)
    when (> (hash-table-count value) 0)
    collect key))

(defun all-uuids ()
  "Returns a list of all uuids of the triples in the triplestore."
  (collect-hash-keys *ts*))

(defun all-triples ()
  "Returns a list of all triples in the triplestore."
  (loop for triple being the hash-values of *ts* collect triple))

(defun all-subjects () (collect-hash-keys *spo*))

(defun all-predicates () (collect-hash-keys *pos*))

(defun all-objects () (collect-hash-keys *osp*))

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

(defun remove-entry (index entry)
  (cond ((eq (cddr entry) nil)
	 (setf (gethash (car entry) index)
	       (remove (cadr entry) (gethash (car entry) index) :test #'equal))
	 (when (eq nil (gethash (car entry) index))
	   (remhash (car entry) index)))
	(t (remove-entry (gethash (car entry) index) (cdr entry)))))

(defun remove-from-indicies (triple)
  (with-slots (subject predicate object) triple
    (remove-entry *spo* (list subject predicate object))
    (remove-entry *pos* (list predicate object subject))
    (remove-entry *osp* (list object subject predicate))
    (when (= 0 (hash-table-count (gethash subject *spo*)))
      (remhash subject *spo*))
    (when (= 0 (hash-table-count (gethash predicate *pos*)))
      (remhash predicate *pos*))
    (when (= 0 (hash-table-count (gethash object *osp*)))
      (remhash object *osp*))))

(defun query-index1 (index query)
  (loop for key being the hash-keys of (gethash (first query) index)
     using (hash-value value)
     collect (list (first query) key value)))

(defun query-index2 (index q1 q2)
  (gethash (second q2) (gethash (first q1) index)))

(defun query-index3 (index q1 q2 q3)
  (find (third q3) (query-index2 index q1 q2) :test #'equal))

(defun add-to-indices (triple)
  "Index the triple."
  (with-slots (subject predicate object) triple
    (add-entry *spo* (list subject predicate object))
    (add-entry *pos* (list predicate object subject))
    (add-entry *osp* (list object subject predicate))))

(defun rebuild-indicies ()
  "Literally tries to rebuild every index."
  (setf *spo* (make-hash-table :test 'equal))
  (setf *pos* (make-hash-table :test 'equal))
  (setf *osp* (make-hash-table :test 'equal))
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
	((eq 'uuid (caar parameters)) (gethash (cdar parameters) *ts*))
	(t (filter-triples (remove-if-not (lambda (triple) (equal (slot-value triple (caar parameters))
								  (cdar parameters)))
					  triples)
			   (cdr parameters)))))

(defun remove-triples (parameters)
  "Remove all triples in the triplestore that match the parameters.
Parameters: '((subject NSA) (object evidence))"
  (let* ((triples (filter-triples (all-triples) parameters))
	 (uuids (all-uuids)))
    (mapc #'remove-from-indicies triples)
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
