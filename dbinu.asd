(in-package #:cl-user)
(asdf:defsystem dbinu
    :description "A triplestore database"
    :serial t
    :components ((:file "package")
		 (:file "dbinu"))
    :depends-on (:uuid
		 :cl-conspack))
