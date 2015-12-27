(in-package #:mlsp)

(defparameter *reader-state*
  nil)

(defparameter *current-module*
  nil)

(defgeneric read-decl (head body))

; /////

(defun interpret-decl (expr)
  (read-decl (car expr) (cdr expr)

(defun read-code (stream toplevel-module-name)
  (loop with *reader-state* = nil ; Used in interpret-code
        with *current-module* = nil ; /////
        for expr = (read stream nil nil)
        while expr
        do (interpret-decl expr)
        finally (return *reader-state*)))

(defun read-code-file (filename)
  (with-open-file (stream filename :direction :input)
    (read-code stream)))
