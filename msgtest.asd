;;;; msgtest.asd

(asdf:defsystem #:msgtest
                :description "INSERT PROJECT DESCRIPTION HERE"
                :author "INSERT PROJECT AUTHOR HERE"
                :license "Modified BSD License"
                :serial t
                :depends-on (:CL-NATS
)
                :pathname "./"
                :components ((:file "app-utils")
                             (:file "msgtest")
                             ))

