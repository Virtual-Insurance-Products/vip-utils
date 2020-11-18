
(in-package :vip-utils)



;; simple memoization. This is something which has been implemented many times by people. I'll just make a macro to wrap defun...

(defvar *defun-memo* (make-hash-table))

(defun clear-function-memo (name)
  (setf (gethash name *defun-memo*)
        (make-hash-table :test #'equal)))

;; I suppose the body should only really be a single form since we should not have side effects, but I'm not going to enforce that lexically...
(defmacro defun/m (name arg-list &body body)
  "Like defun but defines a memoizing function"
  (dolist (a '(&rest &body &key))
    (when (member a arg-list)
      (error "I haven't implemented memoized ~A parameters yet." a)))


  (let ((memo (gensym))
        (memoized-value (gensym "value"))
        (memoized-value-found (gensym "found")))
    `(progn
       (clear-function-memo ',name)

       (defun ,name ,arg-list
         ;; then we have to check for memoized answer...
         (let ((,memo (gethash ',name *defun-memo*)))
           (multiple-value-bind (,memoized-value ,memoized-value-found)
               (gethash (list ,@arg-list)
                        ,memo)
             (if ,memoized-value-found
                 ,memoized-value
                 (setf (gethash (list ,@arg-list)
                                ,memo)
                       (progn
                         ,@body)))))))))

;; This is like the above but must yield a non nil value...
(defmacro defun/m* (name arg-list &body body)
  (dolist (a '(&rest &body &key))
    (when (member a arg-list)
      (error "I haven't implemented memoized ~A parameters yet." a)))


  (let ((memo (gensym)))
    `(progn
       (clear-function-memo ',name)

       (defun ,name ,arg-list
         ;; then we have to check for memoized answer...
         (let ((,memo (gethash ',name *defun-memo*)))
           (or (gethash (list ,@arg-list)
                        ,memo)
               (setf (gethash (list ,@arg-list)
                              ,memo)
                     (progn
                       ,@body))))))))



;; Clear out all memo tables to free RAM. This is slightly drastic, but not damaging in any way
;; we are consuming huge amounts of RAM here
;; (mapcar #'clear-function-memo (hash-keys *defun-memo*))



(defmacro defun/c (name arglist &body body)
  (cons 'defun
        (cons name
              (cdr (make-curried-lambda arglist body)))))

;; (defun/c plus (a b) (+ a b))
;; (mapcar (plus 2) '(1 2 3))

;; I guess I *could* extend these things to work with optional or key parameters. Currying would only obviously happen if there were omitted required parameters
;; It would be a bit fiddly though.






;; (distinct '(?1 ?1 ?2 ?3))

(defmacro @ (&rest things)
    "Shorthand notation for #'(lambda (a b c) ...) with positional
parameters. (@ + ?1 ?2) expands to #'(lambda (?1 ?2) (+ ?1 ?2))"
    (let ((args nil))
      (arglist things (lambda (x)
                        (push x args)))
      `#'(lambda ,(distinct (sort-arg-list args))
           ,things)))

;; (@ + ?1 2) -> #'(LAMBDA (?1) (+ ?1 2))

;; This one is similar but without the inferred parens.
(defmacro @@ (&rest things)
    "Shorthand notation for #'(lambda (a b c) ...) with positional
parameters. (@@ (+ ?1 ?2)) expands to #'(lambda (?1 ?2) (+ ?1 ?2))"
    (let ((args nil))
      (arglist things (lambda (x)
                        (push x args)))
      ;; although distinct is sort of suppsed to work with strings, and is unnecessarily compute intensive here (a version relying on the sortedness of the arg list would be very much quicker) it works fine.
      `#'(lambda ,(distinct (sort-arg-list args))
           ,@things)))



;; helper macros for writing stuff in CPS...

;; This turns the cdr of (:<< arg-name f1 f2 f3) into (f3 (f2 (f1 arg-name)))
;; note that the f functions are applied in the order given in the arg-list, NOT in the 'inside out' order which is usual
;; ALSO note that the fs can be either symbols representing functions OR compound forms which will be funcalled
;; I hope that this is not confusing
(defun cps-arg-list-to-call (arg-list)
  (reduce (@ if (consp ?2)
                (list 'funcall ?2 ?1)
                (list ?2 ?1)) arg-list))

;; (cps-arg-list-to-call '(a b c d))
;; (cps-arg-list-to-call '(a b (@ (* 2 ?1)) c))

;; this is useful. It's like the :<< arguments in the cps-seq form.
;; It's handy for point free programming
;; funnily enough it gives word sequences much like forth words.
(defmacro compose (&rest functions)
  `(lambda (*)
     ,(cps-arg-list-to-call (cons '* functions))))

;; This might be in the opposite order of normal function composition
;; (compose f g)
;; yes it is - g o f == (compose f g)


;; also, in case you want to defun a function which is just a simple composition of some other functions...
(defmacro defun* (name &body x)
  `(defun ,name (*)
     ,(cps-arg-list-to-call (cons '* x))))


;; given an argument list and a body make a lambda
;; as well as containing normal formal parameters the argument list can contain function mappings...
(defun lambda-for-cps (arguments body)
  (let ((arguments (if (or (symbolp arguments)
                           (and (consp arguments)
                                (eq (first arguments) :<<)))
                       (list arguments)
                       arguments)))

    ;; now we have to get the normal formal parameter list...
    `(lambda ,(mapcar (@ if (consp ?1)
                       (second ?1) ; the last thing is the actual argument name
                       ?1)
          arguments)
       ;; now we need to wrap a let if necessary...
       ,@(let ((letted (loop for arg in arguments
                             when (consp arg)
                               collect (list (second arg)
                                             (cps-arg-list-to-call (cdr arg))))))
           (if letted
               `((let ,letted
                   ,@body))
               body)))))

;; (lambda-for-cps '(:<< x cdr) '(c d))

(defun lambda-list-replace (tree &key
                                   (replace '_)
                                   (with (lambda () (gensym))))
  "Replaces all occurrences of a var with the results of calling with.
We have to be careful what we replace. (&optional (a _)) <- here the _ should not be replaced."

  ;; replacements will be mutated as we recurse down the tree, collecting the symbols that have been replaced as we go.
  (let ((replacements '()))
    (labels ((replace-car (param)
               "Replace the parameter in optional and keyword params"
               (if (consp param)
                   (cons (rec (car param))
                         (cdr param))
                   (rec param)))

             (rec (tree)
               "Recursively work down the tree."
               (if (atom tree)
                   ;; Check if it needs replacing.
                   (if (equalp (symbol-name tree) (symbol-name replace))
                       (let ((replaced (funcall with)))
                         (push replaced replacements)
                         replaced) 
                       tree)

                   (let ((element (car tree)))
                     (if (and (symbolp element)
                              (member (symbol-name element) '("&optional" "&key") :test #'equalp))
                         ;; Only replace the cars of the rest of the lambda list
                         (cons element (mapcar #'replace-car (cdr tree)))

                         ;; Work down the tree
                         (cons (rec element) (rec (cdr tree))))))))

      (list (rec tree) replacements))))

(defmacro ignorable-destructuring-bind (lambda-list expr &body forms)
  "A destructuring bind where you can specify particular variables to ignore
by using a _ in the parameter list. These will be explicitly ignored, thus avoiding
a compiler warning."
  (destructuring-bind (new-lambda-list ignores) (lambda-list-replace lambda-list)
    `(destructuring-bind ,new-lambda-list ,expr
       ,@(cons `(declare (ignore ,@ignores)) forms))))



;; new and improved - does the whole job at once rather than relying on repeated macro expansion rounds
;; This allows various transformations of sequences of lisp expressions:-
;; 1. CPS rewrite
;; 2. Monadic 'do' notation
(defun cps-seq-transform (forms)
  (labels ((lambda-args ()
             (if (third forms)
                 (if (consp (third forms))
                     (third forms)
                     ;; we can allow just a single symbol
                     (list (third forms)))
                 (error "Missing continuation arguments!")))

           (symbol-like (a b)
             (and (symbolp a) (symbolp b)
                  (equal (symbol-name a) (symbol-name b))))

           (lambda-body (trail)
             (let ((body (cps-seq-transform trail)))
               (if (symbol-like (first body) 'progn)
                   (cdr body)
                   (list body)))))

    (if (cdr forms)
        (cond ((eq (second forms) :=)
               (append (first forms)
                       (list (lambda-for-cps (lambda-args)
                                             (lambda-body (cdddr forms))))))

              ((eq (second forms) :-)
               (cons (car (first forms))
                     (cons (lambda-for-cps (lambda-args)
                                           (lambda-body (cdddr forms)))

                           (cdr (first forms)))))

              ;; when we don't care what the monadic value is...
              ((symbol-like (first forms) '-) (cps-seq-transform `(_ <- ,(cadr forms) (declare (ignore _)) ,@(cddr forms))))

              ((symbol-like (second forms) '<-)
               (if (consp (first forms))
                   (cps-seq-transform `((bind ,(third forms)) := *

                                        (ignorable-destructuring-bind ,(first forms)
                                            *
                                          ,(cps-seq-transform (cdddr forms)))))
                   (cps-seq-transform `((bind ,(third forms)) := ,(first forms) ,@(cdddr forms)))))


              ((eq (second forms) :==)
               (if (consp (third forms))
                   `(flet ((,(first (third forms)) ,(cdr (third forms))
                             ,(first forms)))
                      ,(cps-seq-transform (cdddr forms)))
                   `(let ((,(third forms) ,(first forms)))
                      ,(cps-seq-transform (cdddr forms)))))


              ;; supply identity function as a convenience
              ((eq (first forms) :=)
               (append (second forms) `((lambda (*) *))))

              ((and (symbolp (second forms))
                    (not (or (symbol-like (second forms) '-)
                             (symbol-like (third forms) '<-))))
               (cps-seq-transform `((,(second forms) ,(first forms)) ,@(cddr forms))))

              ;; This now flattens out some redundant nested progns for ease of testing
              (t (let ((rest (cps-seq-transform (cdr forms))))
                   (if (eq (car rest) 'progn)
                       `(progn ,(car forms)
                               ,@(cdr rest))
                       `(progn ,(car forms)
                               ,(cps-seq-transform (cdr forms)))))))

        (car forms))))

;; !!! I've never really used this one. Best to ignore it. 
(defun monadic-cps-seq-transform (forms)
  (flet ((symbol-like (a b)
           (and (symbolp a) (symbolp b)
                (equal (symbol-name a) (symbol-name b)))))
    (if (cdr forms)
        (let ((second (second forms)))
          (cond ((symbol-like second '<-)
                 `(bind ,(third forms) #'(lambda (,@(if (consp (first forms))
                                                   (first forms)
                                                   (list (first forms))))
                                           ,(monadic-cps-seq-transform (cdddr forms)))))

                ;; now 'assignment'...
                ;; (definition really)
                ((symbol-like second '=)
                 (if (consp (first forms))
                     `(flet ((,(first (first forms)) ,(cdr (first forms))
                               ,(third forms)))
                        ,(monadic-cps-seq-transform (cdddr forms)))
                     `(let ((,(first forms) ,(third forms)))
                        ,(monadic-cps-seq-transform (cdddr forms)))))

                (t `(bind ,(first forms) #'(lambda (_)
                                             (declare (ignore _))
                                             ,(monadic-cps-seq-transform (cdr forms)))))
                ))
        (car forms))))



;; This was in place of (list (lambda-for-cps ...
;; `((lambda ,(lambda-args)
;;          (cps-seq ,@(cdddr forms))))

;; This is kind of experimental - I'm not sure how useful it will prove to be
;; It seems as though it could make CPS calls look a lot cleaner though.
;; This is mainly to stop my code from wandering too far to the right!
;; !!! Add a :- form to put the k as the first arg. Then I can do:-
;; (mapcar some-list) :- (item)
;; (do-something-with-the-item)
;; even nested mappings and whatnot...
;; DEPRECATED:- (doesn't just do cps)
(defmacro cps-seq (&body forms)
  (cps-seq-transform forms))

;; just the same as above. It's not really just doing CPS transforms for me now - It's doing other useful things too.
;; I can program in RPN if I want to.
(defmacro seq (&body forms)
  (if (eq (car forms) :do)
      ;; A new, more monad oriented transformer...
      ;; !!! This is still experimental and hasn't really been battle tested
      (monadic-cps-seq-transform (cdr forms))
      (cps-seq-transform forms)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Threading macros

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun insert-into-position (form sym pos)
    (append (subseq form 0 (1- pos))
            (list sym)
            (subseq form (1- pos)))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun thread-forms (forms insertfn)
    (let ((sym (gensym "thread")))
      `(let* ((,sym ,(first forms))
              ,@(mapcar (lambda (form)
                          `(,sym ,(funcall insertfn
                                           (if (listp form) form (list form))
                                           sym)))
                        (rest forms)))
         ,sym))))

(defmacro ~> (&rest forms)
  "Thread first. Evaluates the first form and passes its result as the first parameter to the second form.
The result of this is passed as the first parameter to the next form and so on. The result of the final form
is returned.

Instead of (+ (/ (* x 4) 2) 5)

write (~> (* x 4) (/ 2) (+ 5))"
  (thread-forms forms (lambda (form sym)
                        (insert-into-position form sym 2))))

(defmacro ~>> (&rest forms)
  "Thread last. Evaluates the first form and passes its result as the final parameter to the second form.
The result of this is passed as the final parameter to the next form and so on. The result of the final form
is returned.

Instead of (+ 5 (/ 2 (* x 4)))

write (~>> (* x 4) (/ 2) (+ 5))"
  (thread-forms forms (lambda (form sym)
                        (insert-into-position form sym (1+ (length form))))))


(defmacro as~> (name exp &rest forms)
  "Thread wherever. The first param is used as a position marker. Evaluates the first form and passes its result
as the parameter in the position specified by the position marker. The result is passed to the next form in the position
specified by the position marker ther and so on. The result of the final form is returned.

Instead of (+ 5 (/ (* x 4) 2))

write (as~> % (* x 4) (/ % 2) (+ 5 %))"
  `(let* ((,name ,exp)
          ,@ (mapcar (@ list name ?1)
               forms))
     ,name))



(defmacro casep (key &rest clauses)
  "Like case, but does an equalp test instead of just an eq, 
so we can switch on strings."
  (let ((key* (gensym)))
    `(let ((,key* ,key))
       (cond
         ,@(mapcar (lambda (clause)
                     (let ((test (car clause))
                           (forms (cdr clause)))
                       `(,(if (eq test 'otherwise)
                              't
                              `(member ,key* (list ,@(if (listp test)
                                                         test
                                                         (list test))) :test #'equalp))
                          ,@forms)))
                   clauses)))))

(defmacro call-next-method? (&rest a)
  "Call next method if there is one, otherwise do nothing."
  `(when (next-method-p)
     (call-next-method ,@a)))



(defmacro with-partially-applied (functions arg &body body)
  (let ((bound (if (consp arg)
                   (gensym "result")
                   arg)))
    (let ((inner `(flet ,(mapcar (lambda (f)
                                   `(,(if (listp f)
                                          (second f)
                                          f) (&rest r)
                                     (apply ,(if (listp f)
                                                 (first f)
                                                 f)
                                            (cons ,bound r))))
                                 (if (listp functions)
                                     functions (list functions)))
                    ,@body)))
      (if (consp arg)
          `(let ((,bound ,arg))
             ,inner)
          inner))))




(define-condition expression-type-error (type-error)
  ((expression :initarg :expression :reader type-error-expression)
   (message :initarg :message :reader type-error-message :initform nil))
  (:default-initargs :expression (error "Missing expression for type error")))

(defmethod print-object :around ((x expression-type-error) (s stream))
  (if (type-error-message x)
      (format s "~A" (type-error-message x))
      (call-next-method)))

(defmethod print-object :before ((x expression-type-error) (s stream))
  (let ((*print-pretty* nil))
    (format s "In the expression ~A: " (type-error-expression x))))

;; (error 'expression-type-error :datum nil :expected-type 'integer :expression '(do-something 1 2 3))

(defmacro ensure-type (exp &optional (type ''(not null))
                             error-message)
  (let ((value (gensym "value")))
    `(let ((,value ,exp))
       (unless (typep ,value ,type)
         ;; define an error type for this
         (error 'expression-type-error
                :message ,error-message
                :datum ,value
                :expected-type ,type
                :expression ',exp))
       ,value)))

;; (ensure-type nil)
;; (ensure-type (+ 1 2) 'string)
;; (ensure-type (+ 1 2) 'string "Adding two numbers doesn't give a string")

(defun make-let-expression (let/let* bindings body)
  `(,let/let* (,@ (loop for (var exp type error) in bindings
                        collect (list var `(ensure-type ,exp ,(or type ''(not null))
                                                        ,error))))
              ,@body))

(defmacro tlet ((&rest bindings) &body body)
  (make-let-expression 'let bindings body))

(defmacro tlet* ((&rest bindings) &body body)
  (make-let-expression 'let* bindings body))

;; (tlet* ((x (+ 1 2 3)) (y (when (> x 10) (1+ x)))) (list x y))

