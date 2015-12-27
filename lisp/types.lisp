(in-package #:mlsp)

(defclass named ()
  ((name :accessor name
         :initarg :name
         :initform ""
         :type string)))

(defclass module (named)
  ((decl :accessor module-decl
         :initarg :decl
         :initform nil
         :type list)))

(defclass func (named)
  ())

(defclass type (named)
  ())
