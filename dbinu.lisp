(in-package :dbinu)

(defstruct triple
  subject
  predicate
  object
  graph
  id)

(defun make-triplestore () (make-hash-table))

(defun add-triple (triplestore s p o)
  "Creates a triple and stores it to the triplestore with a UUID key."
  (let ((triple-id (make-v1-uuid)))
    (setf (gethash triple-id triplestore)
	  (make-triple :subject s :predicate p :object o :id triple-id))))

(defun all-triples (triplestore)
  "Return all triples in the triplestore in the form of a list."
  (loop for triple being the hash-values of triplestore
     using (hash-key key)
     collect triple))

(defun filter-triples (triples &optional query)
  "Return all triples in a list that match the query.
Usage: (filter-triples (all-triples *my-triplestore*) '((predicate has-tag) (object yo)))"
  (cond ((eq query nil) triples)
	(t (filter-triples (remove-if-not (lambda (triple) (equal (slot-value triple (caar query))
								  (cadar query)))
					  triples)
			   (cdr query)))))

(defun remove-triples (triplestore query)
  "Remove all triples in the triplestore that match the query. See filter-triples for an example query."
  (let ((triple-ids (loop for triple in (filter-triples (all-triples triplestore) query)
		       collect (triple-id triple))))
    (mapc (lambda (id) (remhash id triplestore)) triple-ids)))

(defun backup-triplestore (triplestore filepath)
  (with-open-file (out filepath
		       :direction :output
		       :if-exists :supersede
		       :element-type '(unsigned-byte 8))
    (write-sequence (encode triplestore) out)))

(defun load-triplestore (filepath)
  (decode-file filepath))
