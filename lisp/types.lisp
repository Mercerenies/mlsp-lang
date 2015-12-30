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

(defclass basic-func (named)
  ())

(defclass basic-type (named)
  ((parent :accessor type-parent
           :initarg :parent
           :initform nil
           :type type-spec)
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
      (format stream "name=~S args=~S parent=~A vars=~A"
              name args parent vars))))
#|
(deftype type-value ()
  '(or type-spec (eql infer)))

(deftype type-access ()
  '(or read write))

(defclass type-spec ()
  ((access :accessor type-access
           :initarg :access
           :initform 'read
           :type type-access)))

(defclass type-tuple (type-spec)
  ((constituents :accessor type-tuple-elem
                 :initarg :elem
                 :initform nil
                 :type list)))

(defclass type-named (named type-spec)
  ((constituents :accessor type-named-elem
                 :initarg :elem
                 :initform nil
                 :type list)))

(defclass type-func (type-spec)
  ((arguments :accessor type-func-args
              :initarg :args
              :initform nil
              :type list)
   (result :accessor type-func-result
           :initarg :result
           :initform nil
           :type type-tuple)))
|#
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
                       :message "decorated package name is not allowed")
            and do (loop-finish)
        when curr
            do (push package (module-decl curr))
        do (setf curr package)
        do (push curr hier)
        finally (return (or hier (list (make-default-package))))))
