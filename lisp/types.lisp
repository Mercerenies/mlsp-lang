(in-package #:mlsp)

(defclass named ()
  ((name :accessor name
         :initarg :name
         :initform ""
         :type string)))

(defclass module (named)
  ())

(defclass function-decl (named)
  ())
