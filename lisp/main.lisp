(in-package #:mlsp)

(defun initial-read (filename)
  "Performs the initial read to establish the Common Lisp data structures from the
   raw S-expressions provided in the file."
  (let ((errors nil)
        (warnings nil)
        (*print-escape* nil)
        (result nil))
    (handler-bind
        ((code-error #'(lambda (c)
                         (format *error-output* "~A~%" c)
                         (push c errors)))
         (code-warning #'(lambda (c)
                           (format *error-output* "~A~%" c)
                           (push c warnings))))
      (setf result (start-read-code filename)))
    (values result errors)))

(defun compile-code (filename)
  (multiple-value-bind (data data-clean?) (initial-read filename)
    data))
