(in-package :cl-user)

(defpackage dbinu
  (:use :cl :uuid :conspack)
  (:export :make-ts
	   :save-ts
	   :load-ts
	   :add-triples
	   :subject
	   :predicate
	   :object
	   :graph
	   :uuid
	   :all-triples
	   :filter-triples
	   :remove-triples
	   ))
