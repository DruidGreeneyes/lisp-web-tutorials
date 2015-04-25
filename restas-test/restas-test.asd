;;;; restas-test.asd

(defpackage #:restas-test-config (:export #:*base-directory*))
(defparameter restas-test-config:*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:restas-test
  :description "Describe restas-test here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:restas
               #:sexml
               #:postmodern
               #:ironclad
               #:babel)
  :serial t
  :components ((:file "defmodule")
               (:file "util")
               (:file "template")
               (:file "restas-test")))

