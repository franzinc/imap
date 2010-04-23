;;; ASD file contributed by james anderson <james.anderson@setf.de>

(in-package :cl-user)

(unless (find-class 'asdf::cl-file nil)
  (defclass asdf::cl-file (asdf:cl-source-file) ())
  (defmethod asdf:source-file-type ((c asdf::cl-file) (s asdf:module)) "cl"))

(asdf:defsystem :imap
  :serial t
  :components
  ((:file "imap")
   (:file "smtp")))
