(in-package :vip-utils)

(define-condition invalid-form-value (error)
  ((form-field-name :initarg :form-field-name :reader form-field-name)
   (value :initarg :value :reader value)))

(defmethod print-object ((object invalid-form-value) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Invalid form value: ~A"
            (if (slot-boundp object 'value)
                (value object)
                "???" ))))
