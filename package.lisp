(in-package #:cl-user)
(defpackage #:dbinu
  (:use #:cl #:uuid #:conspack)
  (:export #:make-ts
	   #:save-ts
	   #:load-ts
	   
	   #:add-triple
	   #:add-triples
	   
	   #:all-triples
	   #:all-subjects
	   #:all-predicates
	   #:all-objects
	   #:all-uuids
	   
	   #:subject
	   #:predicate
	   #:object
	   #:graph
	   #:uuid

	   #:*ts*
	   #:*spo*
	   #:*pos*
	   #:*osp*
	   
	   #:query-index1
	   #:query-index2
	   #:query-index3
	   #:rebuild-indicies
	   
	   #:filter-triples
	   #:remove-triples))
