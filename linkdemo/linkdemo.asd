;;;; linkdemo.asd

(defpackage #:linkdemo-config (:export #:*base-directory*))
(defparameter linkdemo-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:linkdemo
  :description "Describe linkdemo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:restas
               #:sexml
               #:postmodern
               #:ironclad
               #:babel)
  :serial t
  :components ((:file "defmodule")
               (:file "pg-datastore")
               (:file "util")
               (:file "template")
               (:file "linkdemo")))