(in-package #:mlsp)

; Borrowed from Rosetta Code (http://rosettacode.org/wiki/Tokenize_a_string#Common_Lisp)
(defun string-split (string token)
  (loop for start = 0 then (1+ finish)
        for finish = (position token string :start start)
        collecting (subseq string start finish)
        until (null finish)))
