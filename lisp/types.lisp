(in-package #:mlsp)

(defclass named ()
  ((name :accessor name
         :initarg :name
         :initform ""
         :type string)))

(defclass basic-module (named)
  ((decl :accessor module-decl
         :initarg :decl
         :initform nil
         :type list)))

(defclass basic-package (basic-module)
  ())

(defmethod print-object ((obj basic-module) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S ~S"
            (name obj) (module-decl obj))))

(defmethod print-object ((obj basic-package) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S ~S"
            (name obj) (module-decl obj))))

(defclass basic-func (named)
  ())

(defclass basic-type (named)
  ())

(defun make-basic-package (name)
  (make-instance 'basic-package :name name))

(defun make-default-package ()
  (make-instance 'basic-package :name "main"))

(defun make-basic-module (name)
  (make-instance 'basic-module :name name))

; Create a package hierarchy based on the named in the identifier
; Uses *packages*
(defun package-hierarchy (name)
  (loop with hier = nil
        with curr = nil
        for part in (tokenize name)
        for package = (or (find part *packages* :test #'string= :key #'name)
                          (make-basic-package part))
        unless (is-plain-id part)
            do (signal 'name-error
                       :message "decorated package name is not allowed"
                       :pos *source-pos*)
            and do (loop-finish)
        when curr
            do (push package (module-decl curr))
        do (setf curr package)
        do (push curr hier)
        finally (return (or hier (list (make-default-package))))))
