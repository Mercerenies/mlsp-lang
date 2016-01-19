(in-package #:mlsp)

(defgeneric error-contents (err)
  (:documentation "Produces a user-friendly string describing the error or condition
                   in words. The argument should be a code-condition and the generic
                   implementation should return a string."))

(define-condition code-condition (simple-condition)
  ((source-pos :accessor error-pos
               :initform *source-pos*
               :initarg :pos) ; should have form (row col)
   (prefix :accessor error-prefix
           :initform "Error"
           :initarg :prefix)
   (message :accessor error-message
            :initform "Error"
            :initarg :message))
  (:report (lambda (c s)
             (format s "line ~D col ~D:~%  ~A"
                     (first (error-pos c)) (second (error-pos c)) (error-contents c))))
  (:documentation "An error or warning as a result of the language code that was
                   passed in, NOT a condition as a result of the Lisp program itself."))

(define-condition code-error (code-condition)
  ()
  (:report (lambda (c s)
             (format s "line ~D col ~D:~%  ~A"
                     (first (error-pos c)) (second (error-pos c)) (error-contents c))))
  (:documentation "A code error serious enough to warrant failing compilation. Code that
                   signals this error should make every effort to ignore the problem and
                   replace erroneous code with something logical to allow other errors
                   to be accumulated before outright failing."))

(define-condition code-warning (code-condition)
  ()
  (:report (lambda (c s)
             (format s "line ~D col ~D:~%  ~A"
                     (first (error-pos c)) (second (error-pos c)) (error-contents c))))
  (:documentation "A notification of potentially bad or erroneous code that does
                   not constitute an error or a compile failure on its own."))

(defmethod error-contents ((err code-condition))
  (format nil "~A: ~A"
          (error-prefix err) (error-message err)))

(define-condition verify-error (code-error)
  ()
  (:default-initargs :prefix "verify error")
  (:documentation "A verify error indicates a flaw in the incoming S-expressions
                   that makes them invalid or unreadable, such as a non-list where
                   a list was expected or the wrong number of arguments to a syntactic
                   construct."))

(define-condition name-error (code-error)
  ()
  (:default-initargs :prefix "name error")
  (:documentation "An error as a result of a name or identifier, such as a name
                   conflict between two separate objects or an invalid name caused
                   by the wrong sort of sigil or suffix on an identifier."))

(define-condition unsupported-warning (code-warning)
  ()
  (:default-initargs :prefix "unsupported warning")
  (:documentation "A warning signaled as a result of a syntactic construct that
                   has not been implemented yet. The statement or expression that
                   caused this warning should be ignored and regarded as a future
                   construct that the language does not yet know how to handle."))
