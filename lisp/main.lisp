(in-package #:mlsp)

(defun initial-read (filename)
  (let ((errors nil)
        (*print-escape* nil)
        (result nil))
    (handler-bind
        ((code-error #'(lambda (c)
                         (format *error-output* "~A~%" c)
                         (push c errors))))
      (setf result (start-read-code filename)))
    (values result errors)))

(defun compile-code (filename)
  (multiple-value-bind (data data-clean?) (initial-read filename)
    data))
