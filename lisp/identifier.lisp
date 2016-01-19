(in-package #:mlsp)

(defun id-properties (id)
  "Computes the properties of the identifier. An identifier can begin in either an
   at-sign (@) or a dollar-sign ($), which result in the properties local-ref and
   dollar-sign, respectively. Likewise, an identifier can end in an exclamation
   mark (!), which results in the property writable. A list of properties is returned."
  (let ((len (length id)))
    (concatenate 'list
                 (if (eql (elt id 0) #\@) '(local-ref) nil)
                 (if (eql (elt id 0) #\$) '(dollar-sign) nil)
                 (if (eql (elt id (1- len)) #\!) '(writable) nil))))

(defun satisfies-id (id is isnt)
  "Computes whether the identifier satisfies ALL of the properties given by the second
   argument and NONE of the properties given by the third. Returns a generalized boolean."
  (let ((props (id-properties id)))
    (and (subsetp is props)
         (null (intersection isnt props)))))

(defun is-plain-id (id)
  "Returns whether the identifier is 'plain', meaning it lacks any sigil prefixes or
   suffixes."
  (satisfies-id id nil '(local-ref dollar-sign writable)))

(defun tokenize (string)
  "Splits a string (usually an identifier) into a list, delimited by the
   dot (.) character."
  (string-split string #\.))
