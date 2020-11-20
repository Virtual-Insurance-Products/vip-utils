
(in-package :vip-utils)

;; A collection of useful CL types (as in typep checkable)

(deftype tuple (&rest x)
  (labels ((f (x)
             (if x
                 `(cons ,(car x) ,(f (cdr x)))
                 'null)))
    (f x)))

;; See https://github.com/exeter-fp/thinking-with-types/blob/master/david/chapter1.org
;; Also Curry Howard isomorphism
(defun cardinality (type &optional (eval t))
  (labels ((cardinality (type)
             ;; Would be good if I could find equivalent for other CL
             ;; Without this type specs won't get expanded
             #+CCL(setf type (ccl::type-expand type))
             (if (listp type)
                 (destructuring-bind (name &rest params)
                     type
                   (ecase name
                     (integer (if (second params)
                                  (1+ (- (second params)
                                         (first params)))
                                  :infinity))
                     (eql 1)
                     (cons `(* ,(cardinality (first params))
                               ,(cardinality (second params))))
                     (member (length params))
                     (or `(+ ,(cardinality (first params))
                             ,(cardinality (second params))))
                     (func (destructuring-bind (a b)
                               params
                             `(expt ,(cardinality b)
                                    ,(cardinality a))))
                     ;; This is a limited special case
                     (and (cond ((and (eql 'not (first (second params)))
                                      (eql 2 (length params))
                                      (subtypep (second (second params)) (first params)))
                                 `(- ,(cardinality (first params))
                                     ,(cardinality (second (second params)))))
                                ((eql 'not (first (first params)))
                                 (cardinality (cons 'and (reverse params))))
                                (t (error "Not handled"))))))
                 (or (case type
                       (nil 0)
                       (null 1)
                       (fixnum (+ most-positive-fixnum (abs most-negative-fixnum) 1))
                       (integer :infinity))
                     (if eval
                         (error "Can't get cardinality of ~A - unkonwn type" type)
                         type)))))
    (awhen (cardinality type)
      (if eval
          (eval it)
          it))))

;; we can use this to determine whether types are 'equivalent' in that they have equal cardinalities.
;; (cardinality '(integer 1 10))
;; -> 10
;; (cardinality '(member))
;; (cardinality 'null)
;; (cardinality '(tuple (member 1 2) (member 2 3)))
;; (cardinality '(and (integer 1 10) (not (eql 4))))
;; (cardinality 'fixnum)

;; I wonder if it's useful and/or sensible to try and work out the cardinality of a CLOS class
