

(in-package :my-blog)



(defpclass blog-post ()
  ((title :initarg :title
          :accessor title)
   (body :initarg :body
         :accessor body)
   (timestamp :initarg :timestamp
              :accessor timestamp
              :initform (get-universal-time)
              :index t)
   (url-part :initarg :url-part
             :accessor url-part
             :initform nil
             :index t)))


(defmethod initialize-instance :after ((obj blog-post) &key)
  "If :url-part wasn't non-nil when making the instance, generate it automatically."
  (cond ((eq nil (url-part obj))
         (setf (url-part obj) (make-url-part (title obj))))))

(defun make-url-part (title)
  "Generate a url-part from a title. A url-part should only have alphanumeric characters or dashes (in place of spaces)."
  (string-downcase 
   (delete-if #'(lambda (x) (not (or (alphanumericp x) (char= #\- x))))
              (substitute #\- #\Space title))))

(defun generate-index-page ()
  "Generate the index page showing all the blog posts."
  (with-output-to-string (stream)
    (let ((html-template:*string-modifier* #'identity))
      (html-template:fill-and-print-template 
       #P"index.tmpl"
       (list :blog-posts 
             (loop for blog-post in (nreverse (get-instances-by-range
                                               'blog-post 'timestamp nil nil))
                collect (list :title (title blog-post)
                              :body (body blog-post)
                              :url-part (url-part blog-post))))
       :stream stream))))

(defun generate-blog-post-page (template)
  "Generate a page using blog post data."
  (let ((url-part (hunchentoot:query-string)))
    (with-output-to-string (stream) ; Create a stream that will give us a sting
      (let ((blog-post (get-instance-by-value 'blog-post 'url-part url-part)) ; Get the right blog post
            (html-template:*string-modifier* #'identity))
        (html-template:fill-and-print-template 
         template
         (cond ((eq nil blog-post)
                (list :title "" :body "" :url-part ""))
               (t (list :title (title blog-post)
                        :body (body blog-post)
                        :url-part (url-part blog-post))))
         :stream stream)))))

(defun view-blog-post-page ()
  "Generate a page for viewing a blog post."
  (generate-blog-post-page #P"post.tmpl"))

(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password) (hunchentoot:authorization)
     (cond ((and (string= username *username*) (string= password *password*))
            ,@body)
           (t (hunchentoot:require-authorization "my-blog")))))

(defun edit-blog-post ()
  (with-http-authentication
      (cond ((eq (hunchentoot:request-method) :GET)
             (generate-blog-post-page #P"post-form.tmpl"))
            ((eq (hunchentoot:request-method) :POST)
             (update-blog-post)))))

(defun create-blog-post ()
  (with-http-authentication
      (cond ((eq (hunchentoot:request-method) :GET)
             (with-output-to-string (stream)
               (html-template:fill-and-print-template #P"post-form.tmpl" nil 
                                                      :stream stream)))
            ((eq (hunchentoot:request-method) :POST)
             (save-new-blog-post)))))

(defun save-new-blog-post ()
  (let ((blog-post (make-instance 'blog-post 
                                  :title (hunchentoot:post-parameter "title")
                                  :body (hunchentoot:post-parameter "body"))))
    (hunchentoot:redirect (url-part blog-post))))

(defun update-blog-post ()
  "Read POST data and modify blog post."
  (let ((blog-post 
         (get-instance-by-value 'blog-post
                                'url-part (hunchentoot:query-string))))
    (setf (title blog-post) (hunchentoot:post-parameter "title"))
    (setf (body blog-post) (hunchentoot:post-parameter "body"))
    (setf (url-part blog-post) (make-url-part (title blog-post)))
    (hunchentoot:redirect (url-part blog-post))))
         

; Set the web server dispatch table
(setq hunchentoot:*dispatch-table*
      (list (hunchentoot:create-regex-dispatcher "^/$" 'generate-index-page)
            (hunchentoot:create-regex-dispatcher "^/view/$"
                                                 'view-blog-post-page)
            (hunchentoot:create-regex-dispatcher "^/edit/$"
                                                 'edit-blog-post)
            (hunchentoot:create-regex-dispatcher "^/create/$"
                                                 'create-blog-post)))

; Make sure html-template looks for files in the right directory
(setq html-template:*default-template-pathname* #P"/Users/vetler/Documents/devel/cl-webapp-intro-part3/source/")

(defun start-blog ()
  "Open the Elephant store and start the web server."
  ; Open the store where our data is stored
  (defvar *elephant-store* (open-store '(:clsql (:sqlite3 "/tmp/blog.db"))))
  ; Start the web server
  (defvar *ht-server* (hunchentoot:start-server :port 8080)))

(defun stop-blog ()
  "Close the Elephant store and stop the web server."
  (close-store *elephant-store*)
  (hunchentoot:stop-server *ht-server*))

; Username and password for admin access
(defvar *username* "admin")
(defvar *password* "secret")
