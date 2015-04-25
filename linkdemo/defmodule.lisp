;;;; defmodule.lisp
(restas:define-policy datastore
  (:interface-package #:linkdemo.policy.datastore)
  (:interface-method-template "DATASTORE-~A")
  (:internal-package #:linkdemo.datastore)
  (:internal-function-template "~A")
  
  (define-method init ()
    "initiate the datastore")
  
  (define-method find-user (username)
    "find the user by name")
    
  (define-method auth-user (username password)
    "check if a user exists and has the supplied password")
  
  (define-method register-user (username password)
    "register a new user")
  
  (define-method upvoted-p (link-id username)
    "check if a user has upvoted a link")
  
  (define-method upvote (link-id user)
    "upvote a link")
  
  (define-method post-link (url title user)
    "post a new link")
  
  (define-method get-all-links (&optional user)
    "get all of the links in the datastore")
  
  (define-method upvote-count (link-id)
    "get the number of upvotes for a given link"))
    




(restas:define-module #:linkdemo
  (:use #:cl #:restas #:linkdemo.datastore)
  (:export #:start-linkdemo))

(defpackage #:linkdemo.pg-datastore
  (:use #:cl #:postmodern #:linkdemo.policy.datastore)
  (:export #:pg-datastore))
  
  
  

(in-package #:linkdemo)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" linkdemo-config:*base-directory*))

(defparameter *static-directory*
  (merge-pathnames #P"static/" linkdemo-config:*base-directory*))

(defclass acceptor (restas:restas-acceptor)
  ()
  (:default-initargs
   :access-log-destination #P"/home/ubuntu/workspace/src/lisp/linkdemo/logs/access_log"
   :message-log-destination #P"/home/ubuntu/workspace/src/lisp/linkdemo/logs/message_log"))

(sexml:with-compiletime-active-layers
    (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
    (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
    :h))
