(in-package #:mlsp)

; ///// Concepts, instances, functions

(defclass named ()
  ((name :accessor name
         :initarg :name
         :initform ""
         :type symbol)))

(defclass basic-module (named)
  ((decl :accessor module-decl
         :initarg :decl
         :initform (make-hash-table)
         :type hash-table)))

(defmethod print-object ((obj basic-module) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S ~S"
            (name obj) (module-decl obj))))

(defclass basic-package (basic-module)
  ())

(defmethod print-object ((obj basic-package) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S ~S"
            (name obj) (module-decl obj))))

; Object should be named
; Note that as a special case (for convenience), if the object
; is nil, this function will do nothing. Thus, it is safe to
; pass on a return value from a function that returns nil on
; failure to this function.
(defun put-object-in-module (obj module)
  (check-type obj named "a named object")
  (if (gethash (name obj) (module-decl module))
      (signal 'name-error :message "name conflict on ~S" name)
      (setf (gethash (name obj) (module-decl module)) obj)))

(defclass basic-func (named)
  ())

(defclass basic-type (named)
  ((parent :accessor type-parent
           :initarg :parent
           :initform nil
           :type type-expr)
   (args :accessor type-args
         :initarg :args
         :initform nil
         :type list)
   (vars :accessor type-vars
         :initarg :vars
         :initform nil
         :type list) ; Alist
   (fields :accessor type-fields
           :initarg :fields
           :initform nil))) ; TODO Fields type

(defmethod print-object ((obj basic-type) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((args type-args) (parent type-parent)
                     (name name) (vars type-vars))
        obj
      (format stream "name=~S args=~S parent=~S vars=~S"
              name args parent vars))))

(defclass basic-concept (named)
  ((parent :accessor concept-parent
           :initarg :parent
           :initform nil
           :type type-expr) ; Will reference a concept but is syntactically a type expr
   (args :accessor concept-args
         :initarg :args
         :initform nil
         :type list)
   (body :accessor concept-body
         :initarg :body
         :initform nil
         :type list))) ; Alist

(defmethod print-object ((obj basic-concept) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((args concept-args) (parent concept-parent)
                     (name name) (body concept-body))
        obj
      (format stream "name=~S args=~S parent=~S body=~S"
              name args parent body))))

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
                          (make-basic-package (intern part)))
        unless (is-plain-id part)
            do (signal 'name-error
                       :message "decorated package name is not allowed")
            and do (loop-finish)
        when curr
            do (put-object-in-module package curr)
        do (setf curr package)
        do (push curr hier)
        finally (return (or hier (list (make-default-package))))))
