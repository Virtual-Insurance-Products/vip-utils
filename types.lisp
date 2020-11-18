
(in-package :vip-utils)

;; A collection of useful CL types (as in typep checkable)

(deftype tuple (&rest x)
  (labels ((f (x)
             (if x
                 `(cons ,(car x) ,(f (cdr x)))
                 'null)))
    (f x)))
