;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-test-asd
  (:use :cl :asdf))

(in-package :weblocks-test-asd)

(defsystem weblocks-test
    :name "weblocks-test"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "weblocks-test"
    :depends-on (:weblocks)
    :components ((:file "weblocks-test")
                 (:module conf
                  :components ((:file "stores"))
                  :depends-on ("weblocks-test"))
                 (:module src
                  :components ((:file "init-session"))
                  :depends-on ("weblocks-test" conf))))

