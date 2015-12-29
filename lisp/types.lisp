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

(defclass package (named)
  ())

(defclass func (named)
  ())

(defclass basic-type (named)
  ())

; Create a package hierarchy based on the named in the identifier
(defun package-hierarchy (name))
; /////
