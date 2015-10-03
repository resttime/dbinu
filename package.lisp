(in-package :cl-user)

(defpackage dbinu
  (:use :cl :uuid :conspack)
  (:export :make-triplestore
	   :add-triple
	   :all-triples
	   :filter-triples
	   :remove-triples
	   :backup-triplestore
	   :load-triplestore))
