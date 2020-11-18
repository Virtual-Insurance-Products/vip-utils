
(in-package :vip-utils)


;; This is very useful sometimes. Yes, I wrote this before I knew about #'remove-duplicates
(defun distinct (list &key (key #'identity) (test #'equal))
  ;; This is a much better definition
  ;; I'm sorting the results although I don't know why that should really be necessary.
  (let ((results (let ((h (make-hash-table :test test)))
                   (loop for a in list
                         for the-key = (funcall key a)
                         unless (gethash the-key h)
                           collect a
                         do (setf (gethash the-key h) t)))))
    (sort results
          #'string-lessp :key key)))

(defun hash-keys (h)
  (loop for k being the hash-keys of h collect k))



;; Here's an interesting idea - auto currying functions. They behave like normal fixed arity functions if you supply all the parameters, but you can omit later parameters to get partial application
;; This could be really useful if I define functions like this
;; it doesn't support &rest &optional or &key (it would be tricky to support those) but otherwise it's a drop in replacement for defun




;; NOTE - I should fix this to use the supplied-p parameter bit: (lambda (x (y nil y-supplied) (z nil z-supplied)))
(defun make-curried-lambda (arglist body)
  (if (cdr arglist)
      `(lambda (,(first arglist) &optional ,@(cdr arglist))
         (cond (,@(last arglist)
                ,@body)

               ;; then go through each parameter checking to see if it was supplied...
               ,@(loop for param in (cdr (reverse (cdr arglist)))
                       collect `(,param ,(make-curried-lambda (cdr (member param arglist))
                                                              body)))

               ;; then the default case...
               (t ,(make-curried-lambda (cdr arglist) body))
               )

         )
      `(lambda ,arglist ,@body)))




;; These could be useful for writing trivial functions:-
(defun arglist (x accumulate)
  (cond ((consp x)
         (arglist (car x) accumulate)
         (arglist (cdr x) accumulate))
        ((and (symbolp x)
              (scan "^\\?\\d+$" (symbol-name x)))
         (funcall accumulate x))))

(defun sort-arg-list (args)
  (sort args #'(lambda (x y)
                 (< (parse-integer (subseq (symbol-name x) 1))
                    (parse-integer (subseq (symbol-name y) 1))))))


(defun find-all (item sequence &key (test #'eql) key)
  (remove item sequence :test-not test :key key))

(defun merge-sort (list predicate &key (key #'identity))
  "Sort the given list non-destructively."
  (let* ((len (length list))
         (split (floor (/ len 2))))
    (if (<= len 1)
        list
        (merge 'list 
               (merge-sort (subseq list 0 split) predicate :key key)
               (merge-sort (subseq list split) predicate :key key) 
               (lambda (a b) (funcall predicate
                                 (funcall key a)
                                 (funcall key b)))))))


(defun barf (file text &rest params)
  "Append stuff to a file"
  (with-open-file (f file 
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (apply #'format f text params)))

(defun split-lines (text)
  "Split the text into lines."
  (cl-ppcre:split "[\\n\\r]+" text))


(defun string-empty-p (string)
  "Returns true if the string is nil or empty"
  (or (not string)
      (string= (string-trim '(#\Space #\Tab #\Newline) string) "")))

(defun make-same-length (list other-list)
  "Make list the same length as other-list by appending nils."
  (append list
          (make-list (- (length other-list)
                        (length list)))))

(defun collapse-string-alist (list)
  "Collapse an alist that may have duplicate keys down to one that
does not have duplicate keys. Duplicate values are merged and separated by a space."
  (let ((result))
    (loop for (key value) in list
         do
         (aif (find key result :test #'equalp :key #'car)
              (setf (cdr it) (list (concatenate 'string (cadr it) " " value)) )
              (push (list key value) result)))
    result))

(defun is-leap-year (year)
  (and (= 0 (mod year 4))
       (or (not (= 0 (mod year 100)))
           (= 0 (mod year 400)))))


;; Useful function to use https://haveibeenpwned.com/Passwords service
(defun password-pwned-p (password)
  "Use the pwnedpasswords.com service by Troy Hunt to
determine (safely) whether a given password has been pwned. This is
safe to use - it doesn't send the password, nor even a hash *of* the
password, to that service. "
  (let* ((hash (string-upcase
                (ironclad:byte-array-to-hex-string
                 (ironclad:digest-sequence :sha1 (trivial-utf-8:string-to-utf-8-bytes password)))))
         (start (string-upcase (subseq hash 0 5))))

    (or (awhen (second (find hash
                             (mapcar (lambda (x)
                                       (split ":" x))
                                     (split "\\s+" #+nil(drakma:http-request (concatenate 'string "https://api.pwnedpasswords.com/range/"
                                                                                          (string-upcase start)))
                                                   (with-output-to-string (stream)
                                                     (ccl:run-program "curl"
                                                                      (list (concatenate 'string "https://api.pwnedpasswords.com/range/"
                                                                                         (string-upcase start)))
                                                                      :output stream
                                                                      :error *standard-output*))))
                             :key #'first :test (lambda (a b)
                                                  (equal a (concatenate 'string start b)))))
          (if (equal it "1")
              (format nil "This password has been seen once before.")
              (format nil "This password has been seen ~A times before." it)))
        "This password has not been seen in any data breaches. Yet. ")))


(defun apply-to-object (function object)
  "Apply a function to an object such that slots of the object are
used to give values to parameters of the function - either positional
or keyword.

Always applying functions to lists is a bit unimaginative. "

  (let ((slots (mapcar #'ccl:slot-definition-name
                       (ccl:class-slots (class-of object))))
        (keys nil))

    (apply function
           (loop for param in (ccl:arglist function)
                 for slot = (awhen (find param slots :test #'string-equal)
                              (when (slot-boundp object it)
                                it))

                 when (eql param '&key)
                   do (setf keys t)
                 when (and keys slot)
                   collect param
                 when (or (not keys) slot)
                   collect (slot-value object slot)))))
