;;;; defmodule.lisp

(restas:define-module #:restas-test
  (:use #:cl #:restas)
  (:export #:start-blog))

(in-package #:restas-test)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" restas-test-config:*base-directory*))
  
(defparameter *static-tirectory*
  (merge-pathnames #P"static/" restas-test-config:*base-directory*))

(defparameter *posts* nil)

(defclass acceptor (restas:restas-acceptor)
  ()
  (:default-initargs
   :access-log-destination #P"/home/ubuntu/workspace/src/lisp/restas-test/logs/access_log"
   :message-log-destination #P"/home/ubuntu/workspace/src/lisp/restas-test/logs/message_log"))
  
(sexml:with-compiletime-active-layers
  (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
    (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
    :<))