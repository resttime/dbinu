(in-package :cl-user)
(defpackage dbinu
  (:use :cl)
  (:import-from :uuid
		:make-v1-uuid))
(in-package :dbinu)

(defstruct triple
  sub
  pre
  obj
  g
  i)

(defun make-triplestore () (make-hash-table :test 'equal))

(defun add-triple (ts s p o)
  (let ((triple-id (make-v1-uuid)))
    (setf (gethash triple-id ts)
	  (make-triple :sub s
		       :pre p
		       :obj o
		       :i triple-id))))

(defun all-triples (ts)
  (loop for triple being the hash-values of ts
     using (hash-key key)
     collect triple))

(defun filter-sub (triples s)
  (if s
      (remove-if-not (lambda (triple) (equal (triple-sub triple) s)) triples)
      triples))

(defun filter-pre (triples p)
  (if p
      (remove-if-not (lambda (triple) (equal (triple-pre triple) p)) triples)
      triples))

(defun filter-obj (triples o)
  (if o
      (remove-if-not (lambda (triple) (equal (triple-obj triple) o)) triples)
      triples))

(defun filter-i (triples i)
  (if i
      (remove-if-not (lambda (triple) (equal (triple-i triple) i)) triples)
      triples))

(defun filter-triples (ts &key s p o i)
  (filter-obj (filter-pre (filter-sub (filter-i (all-triples ts) i) o) p) s))

(defun remove-triples (ts &key s p o i)
  (let ((triple-ids (loop for triple in (filter-triples ts :s s :p p :o o :i i)
		       collect (triple-i triple))))
    (loop for id in triple-ids
       do (remhash id ts))))
