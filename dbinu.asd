(defsystem dbinu
    :description "A triplestore database"
    :depends-on ("uuid" "cl-conspack")
    :serial t
    :components ((:file "dbinu")))
