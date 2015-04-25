;;;; roeim-blog.asd

(asdf:defsystem #:roeim-blog
  :description "Describe roeim-blog here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:html-template
               #:elephant
               #:clsql)
  :serial t
  :components ((:file "package")
               (:file "roeim-blog")))

