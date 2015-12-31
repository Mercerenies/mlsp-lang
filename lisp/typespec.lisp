(in-package #:mlsp)

; ({} access args...)
; (typename access args...)
; (-> access result args...)

(defun translate-spec (type)
  (unless (and (consp type) (>= (length type) 3))
    (signal 'verify-error :message (format nil "invalid type specifier ~S" type))
    (return-from translate-spec '(t read)))
  (case (first type)
    (tuple-type (let ((*source-pos* (second type)))
                  `({} ,(third type) ,@(mapcar #'translate-spec (cdddr type)))))
    (named-type (let ((*source-pos* (second type)))
                  (if (>= (length type) 4)
                      `(,(intern (third type)) ,(fourth type)
                         ,@(mapcar #'translate-spec (cddddr type)))
                      (prog1 '(t read)
                        (signal 'verify-error
                                :message (format nil "invalid type specifier ~S"
                                                 type))))))
    (func-type (let ((*source-pos* (second type)))
                 (if (>= (length type) 4)
                     `(-> ,(translate-spec (fourth type))
                          ,@(mapcar #'translate-spec (third type)))
                     (prog1 '(t read)
                       (signal 'verify-error
                               :message (format nil "invalid type specifier ~S"
                                                type))))))
    (t (signal 'verify-error :message (format nil "invalid type specifier ~S" type)))))

(defun validate-spec (type)
  (and (consp type)
       (if (eq (first type) '->)
           (>= (length type) 4)
           (>= (length type) 3))
       (member (second type) '(read write))
       (every validate-spec (cddr type))))

(deftype type-expr ()
  '(satisfies validate-spec))

(deftype type-spec ()
  '(cons symbol type-expr))

(defun read-spec (expr)
  (unless (= (length expr) 2)
    (signal 'verify-error :message "invalid type specification")
    (return-from read-spec nil))
  (destructuring-bind (name type) expr
    (cons (intern name) (interpret-type type))))
