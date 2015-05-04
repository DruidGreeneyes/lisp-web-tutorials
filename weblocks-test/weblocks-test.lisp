
(defpackage #:weblocks-test
  (:use :cl :weblocks
        :f-underscore :anaphora)
  (:import-from :hunchentoot #:header-in
                #:set-cookie #:set-cookie* #:cookie-in
                #:user-agent #:referer)
  (:documentation
   "A web application based on Weblocks."))

(in-package :weblocks-test)

(export '(start-weblocks-test stop-weblocks-test))

;; A macro that generates a class or this webapp

(defwebapp weblocks-test
    :prefix "/"
    :description "weblocks-test: A new application"
    :init-user-session 'weblocks-test::init-user-session
    :autostart nil                   ;; have to start the app manually
    :ignore-default-dependencies nil ;; accept the defaults
    :debug t
    )

;; Top level start & stop scripts

(defun start-weblocks-test (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args)
  (start-webapp 'weblocks-test))

(defun stop-weblocks-test ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'weblocks-test)
  (stop-weblocks))

