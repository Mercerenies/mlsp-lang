(in-package #:mlsp)

; Borrowed from Rosetta Code
; (http://rosettacode.org/wiki/Command-line_arguments#Common_Lisp)
(defun argv ()
  (or
   #+clisp (ext:argv)
   #+sbcl sb-ext:*posix-argv*
   #+abcl ext:*command-line-argument-list*
   #+clozure (ccl::command-line-arguments)
   #+gcl si:*command-args*
   #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
   #+cmu extensions:*command-line-strings*
   #+allegro (sys:command-line-arguments)
   #+lispworks sys:*line-arguments-list*
   nil))

; Borrowed from Rosetta Code
; (http://rosettacode.org/wiki/Tokenize_a_string#Common_Lisp)
(defun string-split (string token)
  (loop for start = 0 then (1+ finish)
        for finish = (position token string :start start)
        collecting (subseq string start finish)
        until (null finish)))
