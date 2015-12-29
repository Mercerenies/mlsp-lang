(in-package #:mlsp)

(defun id-properties (id)
  (let ((len (length id)))
    (concatenate 'list
                 (if (eql (elt id 0) #\@) '(local-ref) nil)
                 (if (eql (elt id 0) #\$) '(dollar-sign) nil)
                 (if (eql (elt id (1- len)) #\!) '(writable) nil))))

(defun satisfies-id (id is isnt)
  (let ((props (id-properties id)))
    (and (subsetp is props)
         (null (intersection isnt props)))))

(defun is-plain-id (id)
  (satisfies-id id nil '(local-ref dollar-sign writable)))

(defun tokenize (string)
  (string-split string #\.))
