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
      (signal 'verify-error :message "invalid package header declaration")
      "main")
    (second package-header)))

(defun interpret-type (expr)
  (unless (consp expr)
    (signal 'verify-error :message "expecting non-empty list")
    (return-from interpret-type '*))
  (translate-spec expr))

(defun interpret-fields (expr)
  (unless (null expr)
    (signal 'unsupported-warning
            :message "'fields' declaration not yet implemented"))) ; TODO Fields

(defgeneric read-decl (head body))

(defmethod no-applicable-method ((mthd (eql #'read-decl)) &rest args)
  (declare (ignore args))
  (signal 'verify-error
          :message (format nil "unknown declaration type ~S" (first args))))

(defmethod read-decl ((head (eql 'include)) body)
  (signal 'unsupported-warning
          :message "including is not yet implemented")); TODO Include statements

(defmethod read-decl ((head (eql 'import)) body)
  (signal 'unsupported-warning
          :message "importing is not yet implemented")) ; TODO Import statements

(defmethod read-decl ((head (eql 'module)) body)
  (unless (>= (length body) 2)
    (signal 'verify-error :message "invalid module header")
    (return-from read-decl nil))
  (destructuring-bind (*source-pos* name . decls) body
    (loop with *current-module* = (make-basic-module name)
          for decl in decls
          for res = (interpret-decl decl)
          when res
              do (push res (module-decl *current-module*))
          finally (return *current-module*))))

(defmethod read-decl ((head (eql 'type)) body)
  (unless (= (length body) 6)
    (signal 'verify-error :message "invalid type declaration")
    (return-from read-decl nil))
  (destructuring-bind (*source-pos* name args parent vars fields) body
    ; TODO Check for duplicate fields, etc.
    (make-instance 'basic-type
                   :name name :parent parent
                   :args args
                   :vars (read-vars vars)
                   :fields (interpret-fields fields))))

(defun read-vars (vars) ; ///// Not reading vars properly
  (loop for var in vars
        append (if (/= (length var) 2)
                   (prog1 nil
                     (signal 'verify-error
                             :message "invalid instance var specifier"))
                   (destructuring-bind (name type) var
                     (list (cons name (interpret-type type)))))))

(defun interpret-decl (expr)
  (unless (consp expr)
    (signal 'verify-error :message "expecting non-empty list")
    (return-from interpret-decl nil))
  (read-decl (car expr) (cdr expr)))

(defun read-code (stream package-name)
  (loop with hierarchy = (package-hierarchy package-name)
        with *read-eval* = nil
        with *current-package* = (first (last hierarchy))
        with *current-module* = *current-package*
        with res = nil
        initially (push (first hierarchy) *packages*)
        for expr = (read stream nil nil)
        while expr
        do (setf res (interpret-decl expr))
        when res
            do (push res (module-decl *current-module*))
        finally (return *current-package*)))

(defun read-code-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((pkg (read-package-info stream)))
      (read-code stream pkg))))

(defun start-read-code (filename)
  (let ((*packages* nil)
        (*source-pos* '(0 0)))
    (read-code-file filename)))
