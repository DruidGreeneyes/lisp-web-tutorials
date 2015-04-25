;;;; roeim-blog.lisp

(in-package #:roeim-blog)

;;; "roeim-blog" goes here. Hacks and glory await!

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
             
;;;Add to make-instance so that url-part gets generated
(defmethod initialize-instance :after ((obj blog-post) &key)
  "If :url-part was nil when making the instance, generate it automatically."
  (cond ((eq nil (url-part obj))
         (setf (url-part obj) (make-url-part (title obj))))))

;;;Tell html-template where to find templates.
(setq html-template:*default-template-pathname* #P"/home/ubuntu/workspace/src/lisp/roeim-blog/tmpl/")
(defvar *ht-server* (make-instance 'hunchentoot:easy-acceptor :port 8080))
(defvar *username* "j-box")
(defvar *password* "j-box")

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
      
(defun make-url-part (title)
  "Generate a url-part from a title. a url-part should only have alphanumeric characters or dashes (in place of spaces)."
  (string-downcase
    (delete-if #'(lambda (x) (not (or (alphanumericp x) (char= #\- x))))
               (substitute #\- #\Space title))))

(defun generate-blog-post-page (page-template)
  "Generate a page using blog post data."
  (let ((url-part (hunchentoot:query-string)))
    (with-output-to-string (stream) ; Create a stream that will give us a string
      (let ((blog-post (get-instance-by-value 'blog-post 'url-part url-part))
            (html-template:*string-modifier* #'identity))
        (html-template:fill-and-print-template
          page-template
          (cond ((eq nil blog-post)
                 (list :title "" :body "" :url-part ""))
                (t (list :title (title blog-post)
                         :body (body blog-post)
                         :url-part (url-part blog-post))))
          :stream stream)))))
          
(defun view-blog-post ()
  (generate-blog-post-page #P"post.tmpl"))
  
(defun save-blog-post ()
  "Read POST data and modify blog post."
  (let ((blog-post 
         (get-instance-by-value 'blog-post 
                                'url-part (hunchentoot:query-string))))
    (setf (title blog-post) (hunchentoot:post-parameter "title"))
    (setf (body blog-post) (hunchentoot:post-parameter "body"))
    (setf (url-part blog-post) (make-url-part (title blog-post)))
    (hunchentoot:redirect (url-part blog-post))))
    
(defun save-new-blog-post ()
  (let ((blog-post (make-instance 'blog-post :title (hunchentoot:post-parameter "title")
                                             :body (hunchentoot:post-parameter "body"))))
    (hunchentoot:redirect (url-part blog-post))))
    
(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password) (hunchentoot:authorization)
     (cond ((and (string= username *username*) (string= password *password*))
            ,@body)
           (t (hunchentoot:require-authorization "test-blog")))))
    
(defun create-blog-post ()
  (with-http-authentication
    (cond ((eq (hunchentoot:request-method) :GET)
           (with-output-to-string (stream)
             (html-template:fill-and-print-template #P"post-form.tmpl" nil :stream stream)))
          ((eq (hunchentoot:request-method) :POST)
           (save-new-blog-post)))))

(defun edit-blog-post ()
  (with-http-authentication
    (cond ((eq (hunchentoot:request-method) :GET)
           (generate-blog-post-page #P"post-form.tmpl"))
          ((eq (hunchentoot:request-method) :POST)
           (save-blog-post)))))

;;;Create a dispatcher for the root path/index page, and start the server.
(setq hunchentoot:*dispatch-table*  (list (hunchentoot:create-regex-dispatcher "^/$" 'generate-index-page)
                                          (hunchentoot:create-regex-dispatcher "^/view/$" 'view-blog-post)
                                          (hunchentoot:create-regex-dispatcher "^/edit/$" 'edit-blog-post)
                                          (hunchentoot:create-regex-dispatcher "^/create/$" 'create-blog-post)))

(defun start-blog ()
  "Open the elephant store and start the web server."
  (defvar *elephant-store* (open-store '(:clsql (:sqlite3 "/home/ubuntu/workspace/src/lisp/roeim-blog/data/blog.db"))))
  (hunchentoot:start *ht-server*))
  
(defun stop-blog ()
  (close-store *elephant-store*)
  (hunchentoot:stop *ht-server*))