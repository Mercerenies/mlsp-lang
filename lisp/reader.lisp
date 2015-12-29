(in-package #:mlsp)

(defparameter *packages*
  nil)

(defparameter *current-package*
  nil)

(defparameter *current-module*
  nil)

(defparameter *source-pos*
  '(0 0))

(defun read-package-info (stream)
  (let ((package-header (read stream)))
    (unless (and (listp package-header) (= (length package-header) 2)
                 (eq (first package-header) 'package))
      (signal 'verify-error
              :message "invalid package header declaration"
              :pos *source-pos*)
      "main")
    (second package-header)))

(defgeneric read-decl (head body))

(defmethod no-applicable-method ((mthd (eql #'read-decl)) &rest args)
  (declare (ignore args))
  (signal 'verify-error
          :message (format nil "unknown declaration type ~S" (first args))
          :pos *source-pos*))

(defmethod read-decl ((head (eql 'module)) body)
  (unless (>= (length body) 2)
    (signal 'verify-error :message "invalid module header" :pos *source-pos*)
    (return-from read-decl nil))
  (destructuring-bind (*source-pos* name . decls) body
      (let ((module (make-basic-module name)))
        (push module (module-decl *current-module*))
        (loop with *current-module* = module
              for decl in decls
              do (interpret-decl decl)))))

(defun interpret-decl (expr)
  (unless (consp expr)
    (signal 'verify-error :message "expecting non-empty list" :pos *source-pos*)
    (return-from interpret-decl nil))
  (read-decl (car expr) (cdr expr)))

(defun read-code (stream package-name)
  (loop with hierarchy = (package-hierarchy package-name)
        with *current-package* = (first (last hierarchy))
        with *current-module* = *current-package*
        initially (push (first hierarchy) *packages*)
        for expr = (read stream nil nil)
        while expr
        do (interpret-decl expr)
        finally (return *current-package*)))

(defun read-code-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((pkg (read-package-info stream)))
      (read-code stream pkg))))

(defun start-read-code (filename)
  (let ((*packages* nil)
        (*source-pos* nil))
    (read-code-file filename)))
