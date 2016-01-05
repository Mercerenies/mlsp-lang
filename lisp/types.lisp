(in-package #:mlsp)

; ///// Have a TODO list, future Silvio:
; - Documentation for the rest of the files
; - Concepts need to introduce their functions into the current scope
; - Function declarations themselves (by extension, expressions)

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

(defun put-object-in-module (obj module)
  "Places the named object into the module or package given. If the object
   is non-nil, it should be an instance of named and will be placed in the module
   under its own name, signaling a name-error if there is a name conflict. If the
   object is nil, this function does nothing."
  (when obj
    (check-type obj named "a named object")
    (if (gethash (name obj) (module-decl module))
        (signal 'name-error :message (format nil "name conflict on ~S" (name obj)))
        (setf (gethash (name obj) (module-decl module)) obj))))

(defun put-objects-in-module (obj module)
  "Places the named object or list of named objects into the module or package given."
  (loop for elem in (if (listp obj) obj (list obj))
        do (put-object-in-module elem module)))

(defclass basic-func (named)
  ())

(defclass concept-func (named)
  ((owner :accessor func-concept
          :initarg :concept
          :initform nil
          :type basic-concept)))

(defmethod print-object ((obj concept-func) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "name=~S concept=~S"
            (name obj) (func-concept obj))))

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
   (timing :accessor concept-timing
           :initarg :timing
           :initform 'static
           :type symbol)
   (body :accessor concept-body
         :initarg :body
         :initform nil
         :type list))) ; Alist

; TODO Work on pretty printing if possible
(defmethod print-object ((obj basic-concept) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((args concept-args) (parent concept-parent)
                     (name name) (timing concept-timing) (body concept-body))
        obj
      (format stream "name=~S args=~S parent=~S timing=~S body=~S"
              name args parent timing body))))

(defclass basic-instance (named)
  ((parent :accessor inst-parent
           :initarg :parent
           :initform nil
           :type list)
   (ref :accessor inst-concept
        :initarg :ref
        :initform nil)
   (impl :accessor inst-impl
         :initarg :impl
         :initform nil)
   (args :accessor inst-args
         :initarg :args
         :initform nil
         :type list)
   (body :accessor inst-body
         :initarg :body
         :initform nil
         :type list))) ; Alist

(defmethod print-object ((obj basic-instance) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((args inst-args) (parent inst-parent)
                     (name name) (impl inst-impl) (body inst-body)
                     (ref inst-concept))
        obj
      (format stream "name=~S args=~S ref=~S impl=~S parent=~S body=~S"
              name args ref impl parent body))))

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
