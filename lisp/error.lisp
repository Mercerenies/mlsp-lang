(in-package #:mlsp)

(defgeneric error-contents (err))

(define-condition code-error (simple-condition)
  ((source-pos :accessor error-pos
               :initform nil
               :initarg :pos)
   (prefix :accessor error-prefix
           :initform "Error"
           :initarg :prefix)
   (message :accessor error-message
            :initform "Error"
            :initarg :message))
  (:report (lambda (c s)
             (format s "line ~D col ~D:~%  ~S"
                     (first (error-pos c)) (second (error-pos c)) (error-contents c)))))

(defmethod error-contents ((err code-error))
  (format nil "~S: ~S"
          (error-prefix err) (error-message err)))

(define-condition verify-error (code-error)
  ()
  (:default-initargs :prefix "Verify Error"))
