(in-package #:mlsp)

; ///// Modify the syntax to allow type declarations to take arguments.
;       Necessary to maximize genericism.

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

(defgeneric read-type (head body))

(defmethod no-applicable-method ((mthd (eql #'read-type)) &rest args)
  (declare (ignore args))
  (signal 'verify-error
          :message (format nil "unknown type specifier ~S" (first args))))

(defmethod read-type ((head (eql 'tuple-type)) body)
  (unless (>= (length body) 2)
    (signal 'verify-error :message "invalid tuple type")
    (return-from read-type 'infer))
  (destructuring-bind (*source-pos* access . elems) body
    (unless (typep access 'type-access)
      (signal 'verify-error :message "invalid tuple access")
      (setf access 'read))
    (make-instance 'type-tuple
                   :access access
                   :elem (mapcar #'interpret-type elems))))

(defmethod read-type ((head (eql 'named-type)) body)
  (unless (>= (length body) 3)
    (signal 'verify-error :message "invalid named type")
    (return-from read-type 'infer))
  (destructuring-bind (*source-pos* name access . elems) body
    (unless (typep access 'type-access)
      (signal 'verify-error :message "invalid named type access")
      (setf access 'read))
    (make-instance 'type-tuple
                   :name name
                   :access access
                   :args (mapcar #'interpret-type elems))))

(defmethod read-type ((head (eql 'func-type)) body)
  (unless (= (length body) 3)
    (signal 'verify-error :message "invalid function type")
    (return-from read-type 'infer))
  (destructuring-bind (*source-pos* args result) body
    (make-instance 'type-func
                   :args args
                   :result result)))

(defun interpret-type (expr)
  (unless (consp expr)
    (signal 'verify-error :message "expecting non-empty list")
    (return-from interpret-type 'infer))
  (read-type (car expr) (cdr expr)))

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
  (unless (= (length body) 5)
    (signal 'verify-error :message "invalid type declaration")
    (return-from read-decl nil))
  (destructuring-bind (*source-pos* name parent vars fields) body
    (make-instance 'basic-type
                   :name name :parent parent
                   :args vars :fields (interpret-fields fields))))

(defun interpret-decl (expr)
  (unless (consp expr)
    (signal 'verify-error :message "expecting non-empty list")
    (return-from interpret-decl nil))
  (read-decl (car expr) (cdr expr)))

(defun read-code (stream package-name)
  (loop with hierarchy = (package-hierarchy package-name)
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
