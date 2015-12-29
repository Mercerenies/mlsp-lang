(in-package #:mlsp)

(defgeneric error-contents (err))

(define-condition code-condition (simple-condition)
  ((source-pos :accessor error-pos
               :initform *source-pos*
               :initarg :pos)
   (prefix :accessor error-prefix
           :initform "Error"
           :initarg :prefix)
   (message :accessor error-message
            :initform "Error"
            :initarg :message))
  (:report (lambda (c s)
             (format s "line ~D col ~D:~%  ~A"
                     (first (error-pos c)) (second (error-pos c)) (error-contents c)))))

(define-condition code-error (code-condition)
  ()
  (:report (lambda (c s)
             (format s "line ~D col ~D:~%  ~A"
                     (first (error-pos c)) (second (error-pos c)) (error-contents c)))))

(define-condition code-warning (code-condition)
  ()
  (:report (lambda (c s)
             (format s "line ~D col ~D:~%  ~A"
                     (first (error-pos c)) (second (error-pos c)) (error-contents c)))))

(defmethod error-contents ((err code-condition))
  (format nil "~A: ~A"
          (error-prefix err) (error-message err)))

(define-condition verify-error (code-error)
  ()
  (:default-initargs :prefix "verify error"))

(define-condition name-error (code-error)
  ()
  (:default-initargs :prefix "name error"))

(define-condition unsupported-warning (code-warning)
  ()
  (:default-initargs :prefix "unsupported warning"))
