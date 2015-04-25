(in-package #:linkdemo)

(h:augment-with-doctype "html" "" :auto-emit-p t)

(defun html-frame (context)
  (h:html
    (h:head (h:title (getf context :title)))
    (h:body
      (h:div
        (h:h1 (getf context :title))
        (h:a :href (genurl 'home) "Home") 
        " | "
        (if (logged-on-p)
          (list (h:a :href (genurl 'submit) "Submit a link")
                " | "
                (h:a :href (genurl 'logout)
                     (format nil "Logout ~a" (logged-on-p))))
          (list (h:a :href (genurl 'login) "Log in")
                " or "
                (h:a :href (genurl 'register) "Register")))
        (h:hr))
      (getf context :body))))
      
(defun home-page (links)
  (loop for link in links collect
                            (h:div (if (logged-on-p)
                                       (if (getf link :voted-p)
                                           "*"
                                           (h:a :href (genurl 'upvote-link :id (getf link :id)) "upvote"))
                                       "*")
                                    " "
                                    (getf link :votes)
                                    " "
                                    (h:a :href (getf link :url) (getf link :title)))))

(defun login-form ()
  (h:form :action (genurl 'login/post) :method "post"
          "User name:" (h:br)
          (h:input :type "text" :name "username") (h:br)
          "Password:" (h:br)
          (h:input :type "password" :name "password") (h:br)
          (h:input :type "submit" :value "Log in")))

(defun register-form ()
  (h:form :action (genurl 'register/post) :method "post"
          "User name:" (h:br)
          (h:input :type "text" :name "username") (h:br)
          "Password:" (h:br)
          (h:input :type "password" :name "password") (h:br)
          (h:input :type "submit" :value "Register")))

(defun submit-form ()
  (h:form :action (genurl 'submit/post) :method "post"
          "Title:" (h:br)
          (h:input :type "text" :name "title") (h:br)
          "URL:" (h:br)
          (h:input :type "text" :name "url") (h:br)
          (h:input :type "submit" :value "Submit")))