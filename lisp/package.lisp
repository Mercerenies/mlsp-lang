(defpackage #:mlsp)
(in-package #:mlsp)

; TODO Handle Haskell escape sequences which the Lisp reader won't handle

(load "util.lisp")
(load "error.lisp")
(load "identifier.lisp")
(load "types.lisp")
(load "typespec.lisp")
(load "reader.lisp")
(load "main.lisp")
