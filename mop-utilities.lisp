
(in-package :vip-utils)

(defun make-programmatic-class (superclasses &optional (metaclass (class-of (first superclasses))))
  (make-instance metaclass
                 :name (mapcar #'class-name superclasses)
                 :direct-superclasses superclasses
                 :direct-slots ()))

(defun find-programmatic-class (superclasses)
  (let ((class (find-if #'(lambda (class)
                            (equal superclasses
                                   (ccl:class-direct-superclasses class)))
                        (ccl:class-direct-subclasses (car superclasses)))))
    (if class
        class
        (make-programmatic-class superclasses))))

(defun make-programmatic-instance (superclass-names &rest initargs)
  (apply #'make-instance
         (find-programmatic-class
          (mapcar #'find-class superclass-names))
         initargs))

(defun allocate-programmatic-instance (superclass-names &rest initargs)
  (apply #'allocate-instance
         (find-programmatic-class
          (mapcar #'find-class superclass-names))
         initargs))

(defun string-to-class-name (string &optional (prefix "") &key (package :vip))
  (intern (string-upcase (regex-replace-all "\\s+" (s "~A~A" prefix string) "-")) package))

;; (string-to-class-name "Broker   product" 'db-)

(defun class-name-to-string (class-name)
  (string-capitalize (regex-replace "^DB " (regex-replace-all "-" (symbol-name class-name) " ") "")))

