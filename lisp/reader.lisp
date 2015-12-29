(in-package #:mlsp)

(defparameter *reader-state*
  nil)

(defparameter *current-package*
  nil)

(defparameter *current-module*
  nil)

(defun read-package-info (stream)
  (let ((package-header (read stream)))
    (unless (and (listp package-header) (= (length package-header) 2)
                 (eq (first package-header) 'package))
      (signal 'verify-error :message "invalid package header declaration")
      "main")
    (second package-header)))

(defgeneric read-decl (head body))

(defmethod no-applicable-method ((mthd (eql #'read-decl)) &rest args)
  (declare (ignore args))
  (signal 'verify-error :message (format nil "unknown declaration type ~S" (first args))))

(defmethod read-decl ((head (eql 'module)) body)
  ) ; /////

(defun interpret-decl (expr)
  (unless (consp expr)
    (signal 'verify-error :message "expecting non-empty list")
    (return-from interpret-decl nil))
  (read-decl (car expr) (cdr expr)))

(defun read-code (stream package-name)
  (loop with *reader-state* = nil ; Used in interpret-code
        with *current-package* = (make-instance 'package :name package-name)
        with *current-module* = *current-package*
        for expr = (read stream nil nil)
        while expr
        do (interpret-decl expr)
        finally (return *reader-state*)))

(defun read-code-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((*current-package* (read-package-info stream)))
      (read-code stream filename))))
