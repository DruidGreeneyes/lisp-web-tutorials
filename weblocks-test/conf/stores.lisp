
(in-package :weblocks-test)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *weblocks-test-store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
                   (asdf-system-directory :weblocks-test)))

