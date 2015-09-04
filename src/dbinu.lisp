(in-package :cl-user)
(defpackage dbinu
  (:use :cl)
  (:import-from :uuid
		:make-v1-uuid)
  (:import-from :conspack
		:encode
		:decode-file)
  (:export :triple
	   :triple-subject
	   :triple-predicate
	   :triple-object
	   :triple-graph
	   :triple-id
	   :make-triplestore
	   :add-triple
	   :filter-sub
	   :filter-pre
	   :filter-obj
	   :filter-i
	   :filter-triples
	   :all-triples
	   :remove-triples
	   :backup-triplestore
	   :load-triplestore))
(in-package :dbinu)

(defstruct triple
  subject
  predicate
  object
  graph
  id)

(defun make-triplestore () (make-hash-table))

(defun add-triple (triplestore s p o)
  (let* ((triple-id (make-v1-uuid))
	 (new-triple (make-triple :subject s :predicate p :object o :id triple-id)))
    (setf (gethash triple-id triplestore) new-triple)))

(defun all-triples (triplestore)
  (loop for triple being the hash-values of triplestore
     using (hash-key key)
     collect triple))

(defun filter-sub (triples s)
  (if s
      (remove-if-not (lambda (triple) (equal (triple-subject triple) s)) triples)
      triples))

(defun filter-pre (triples p)
  (if p
      (remove-if-not (lambda (triple) (equal (triple-predicate triple) p)) triples)
      triples))

(defun filter-obj (triples o)
  (if o
      (remove-if-not (lambda (triple) (equal (triple-object triple) o)) triples)
      triples))

(defun filter-i (triples i)
  (if i
      (remove-if-not (lambda (triple) (equal (triple-id triple) i)) triples)
      triples))

(defun filter-triples (triplestore &key s p o i)
  (filter-obj (filter-pre (filter-sub (filter-i (all-triples triplestore) i) s) p) o))

(defun remove-triples (triplestore &key s p o i)
  (let ((triple-ids (loop for triple in (filter-triples triplestore :s s :p p :o o :i i)
		       collect (triple-id triple))))
    (loop for id in triple-ids
       do (remhash id triplestore))))

(defun backup-triplestore (triplestore filepath)
  (with-open-file (out filepath
		       :direction :output
		       :if-exists :supersede
		       :element-type '(unsigned-byte 8))
    (write-sequence (encode triplestore) out)))

(defun load-triplestore (filepath)
  (decode-file filepath))
