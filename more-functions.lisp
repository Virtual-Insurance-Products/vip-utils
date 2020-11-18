
(in-package :vip-utils)



;; !!! I think this is redundant and I should use remove-if
;; I don't know that I really need to do this...
(defun filter (predicate list)
  (mapcan (@ when
              (funcall predicate ?1)
            (list ?1))
          list))

;; This is a lot like: (format t "~{~a~^, ~}" (list 1 2 3)), which I can never remember
(defun string-list (items separator)
  "Concatenate the ITEMS inserting SEPARATOR between each one."
  (apply #'concatenate (cons 'string (loop for (item . rest) on items
                                          collect item
                                          when (and rest separator) collect (string separator)))))

;; ha - this one is slower
#+nil(defun string-list (items separator)
  (with-output-to-string (stream)
    (loop for (item . rest) on items
         do
         (write-sequence item stream)
         when (and rest separator)
         do (write-sequence separator stream)
         )))


;; (string-list (list "one" "two" "three") ", ")


;; I do this so much I get bored of writing it...
(defun s (format-string &rest args)
  (apply #'format `(nil ,format-string ,@args)))



;; this uses /dev/urandom to get a random string. That should be a good source of randomness
(defun random-string (nwords &key (format :hex-string))
  (let ((byte-array (let ((words (make-array nwords :element-type '(unsigned-byte 8))))
                      (with-open-file (rand "/dev/urandom"
                                            :direction :input
                                            :element-type '(unsigned-byte 8))
                        (read-sequence words rand))
                      (if (= nwords 1)
                          (aref words 0)
                          words))))
    ;; this isn't really that useful
    (cond ((eq format :hex-string)
           (ironclad:byte-array-to-hex-string byte-array))
          ((eq format :base64-string)
           (base64:string-to-base64-string (map 'string 'code-char byte-array)))
          ((eq format :byte-array)
           byte-array)
          (t (error "Unkown format: ~A" format)))))



;; (random-string 20)



;; Group a list of items into a list of lists of items where all the items in each sublist compare successfully with the comparator...
;; It assumes that the items are sorted
(defun group (comparator items &key (key (@@ ?1)))
  (when items
    (if (cdr items)
        (let ((rest (group comparator (cdr items) :key key)))
          (if (funcall comparator
                       (funcall key (first items))
                       (funcall key (first (first rest))))
              (cons (cons (first items)
                          (first rest))
                    (cdr rest))
              (cons (list (first items))
                    rest)))
        (list items))))



(defun strip-leading-and-trailing-space (value)
  (regex-replace "^\\s*"
                 (regex-replace "\\s*$" value "")
                 ""))





;; parse a date returning a universal time date which can be inserted into the database
(defun convert-date-format-to-regex (format)
  "Converts the date format like dd/mm/yyyy to a regex with named groups."
  (let ((regex (as~> % format
                     (regex-replace "dd" % "(?<date>\\d\\d?)")
                     (regex-replace "mm" % "(?<month>\\d\\d?)")
                     (regex-replace "yyyy" % "(?<year>\\d\\d\\d\\d)"))))
    regex))

(defun parse-date (x &key (format "dd/mm/yyyy"))
  (let ((regex (convert-date-format-to-regex format)))
    (let ((*allow-named-registers* t))
      (multiple-value-bind (scanner names) (create-scanner regex)
        (multiple-value-bind (match groups) (scan-to-strings scanner x)
          (if match
              (let ((matched (loop for group across groups
                                for name in names
                                collect (cons name (parse-integer group)))))
                (encode-universal-time 0 0 0
                                       (cdr (assoc "date" matched :test #'equal))
                                       (cdr (assoc "month" matched :test #'equal))
                                       (cdr (assoc "year" matched :test #'equal))))
            (error 'invalid-form-value :value x)))))))


(defun parse-datetime (x)
  "Parse a date time in the format dd/mm/yyyy hh:mm
The hh:mm bit is optional."
  
  (unless (scan "^\\d\\d?/\\d\\d?/\\d{4}( \\d{2}:\\d{2})?$" x)
    (error 'invalid-form-value :value x))
  
  (register-groups-bind ((#'parse-integer day month year hour minute))
      ("^(\\d\\d)?/(\\d\\d)?/(\\d{4}) ?(\\d{2})?:?(\\d{2})?$" x)
    (encode-universal-time 0 
                           (or minute 0) 
                           (or hour 0) day month year)))

;; (formatted-date (parse-date "02/1/2007"))



(defun formatted-time (a)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time a)
    (declare (ignore day month year))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour minute second)))

;; (formatted-time (get-universal-time))

(defun formatted-date (a &optional (zero-pad nil))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time a)
    (declare (ignore second minute hour))
    (let ((x (format nil "~A/~A/~A" day month year)))
      (if zero-pad
          (regex-replace "^(\\d)\\/"
                         (regex-replace "\\/(\\d)\\/" x "/0\\1/")
                         "0\\1/")
          x))))

;; (formatted-date (get-universal-time))

(defun formatted-date-time (a)
  (format nil "~A  ~A" (formatted-date a)
          (formatted-time a)))

;; (formatted-date-time (get-universal-time))



(defun trim-string (s)
  (regex-replace "^\\s+"
                 (regex-replace "\\s+$" s "")
                 ""))

;; parse a fractional number but AS A REAL - not a float
;; This will be useful for doing comparisons with the current value. I don't really like how this works - it's all regexy
;; Maybe it would have been better to tokenize it first?
(defun parse-rational (price)
  (setf price (regex-replace (s "~A" #\Pound_Sign) (trim-string price) ""))
  (unless (scan "^-?\\d+(\\.\\d+)?$" price)
    (error 'invalid-form-value :value price))
  (let ((tokens (split "\\." price)))
    (awhen (mapcar #'parse-integer tokens)
      (+ (first it)
         (/ (if (scan "^-" price)
                (- 0 (or (second it) 0))
                (or (second it) 0))
            (expt 10 (length (second tokens))))))))



(defun zero-pad (n digits)
  (labels ((f (s)
             (if (< (length s) digits)
                 (f (concatenate 'string "0" s))
                 s)))
    (f (s "~A" n))))







;; this should really be called 'round-up' or something. It's the way
;; I was always taught to do it at school, but there are (several)
;; other ways. In fact, this is probably not the best.
(defun sensible-round (x)
  (let* ((sign x)
         (x (abs x))
         (ans (cond ((= (mod x 1) 0)
                     x)
                    ((< (mod x 1) 1/2)
                     (truncate x))
                    ((> x 0)
                     (ceiling x))
                    (t (error "Eh? ~A" x)))))
    (if (> sign 0)
        ans
        (- ans))))


(defun round-price-to-nearest-penny (price)
  (/ (sensible-round (* price 100))
     100))



;; this complicated function is needed to avoid coercion to floats when presenting things
;; This seems complicated doesn't it?
(defun large-decimal-expansion (n &key (maximum-digits 12) (minimum-digits 0))
  (multiple-value-bind (whole-part fraction-part)
      (floor (abs n))
    (concatenate 'string
                 (if (< n 0) "-" "")
                 (format nil "~A" whole-part)
                 (if (= fraction-part 0)
                     (if (> minimum-digits 0)
                         (with-output-to-string (stream)
                           (write-char #\. stream)
                           (loop for i from 1 to minimum-digits
                              do (write-char #\0 stream)))
                         "")
                     (let ((p (concatenate 'string
                                           "."
                                           (regex-replace "(.)0+$"
                                                          (format nil (format nil "~~~A,'0D" maximum-digits)
                                                                  (floor (* (expt 10 maximum-digits)
                                                                            fraction-part)))
                                                          "\\1"))))
                       (concatenate 'string
                                    p
                                    (if (> minimum-digits (- (length p) 1))
                                        (with-output-to-string (stream)
                                          (loop for i from 0 to (- minimum-digits (length p))
                                             do (write-char #\0 stream)))
                                        "")))))))

;; This can be used in format strings: ~/dollar/
;; it doesn't suffer from rounding problems
;; Note - DO NOT USE ~$ TO OUTPUT CURRENCIES. It rounds pennies when you have a few hundred thousand pounds!!!
(defun common-lisp-user::dollar (stream number a b)
  (declare (ignore a b))
  ;; This rounds the price to the nearest penny first. That gives it slightly different behaviour with, for example, 2/3
  (write-sequence (large-decimal-expansion (round-price-to-nearest-penny number) :maximum-digits 2 :minimum-digits 2)
                  stream))





(defun shallow-copy-object (original &optional update)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'ccl:slot-definition-name (ccl:class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    (when update
      (funcall update copy))
    copy))

;; !!! It might (arguably) be better if this could use setters if they are defined
(defun update-slots (object &rest slots-and-values)
  (shallow-copy-object object
                       (lambda (new)
                         (loop for (slot value) on slots-and-values by #'cddr
                              do (setf (slot-value new slot) value)))))





;; a version of this which works in both CCL and SBCL the same way. The two implementations of this are virtually identical anyway.
(defun run-program (command args &key wait input (output *standard-output*) (error nil))
  #+ccl(ccl:run-program command args :wait wait :output output :error error :input input)
  ;; passing :search t makes this behave the same
  #+sbcl(sb-ext:run-program command args :wait wait :output output :error error :input input :search t))



;; for executing stuff through a shell (bash in this case)
(defun shell (command &key (split-lines t) input)
  (awhen (with-output-to-string (stream)
           (run-program "bash" (list "-c" command)
                        :wait t
                        :input input
                        :output stream))
    (if split-lines
        (split "\\n" it)
        it)))

;; (shell "ls /bin | grep a" )


(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

;; (flatten '(1 2 3 (43 (5 6) 8 9)))



;; for actually writing hashes in the code this is a simpler notation
(defun hash (values &key (test #'equal) (size 60))
  (let ((h (make-hash-table :test test :size size)))
    (loop for (key value) on values by #'cddr
       do (setf (gethash key h)
                value))
    h))


(defun hash-values (hash)
  (loop for v being the hash-values in hash collect v))

;; note that we can do this if we want to read hashes:-
;; (list 1 2 3 #[hash (:a "one" :b "two")])
;; this will work inside quoted expressions and it works at read time.


(defun hash-keys-and-values (hash)
  (let ((x nil))
    (maphash (@ push (list ?1 ?2) x) hash)
    x))

;; (hash-keys-and-values (hash (list :A 1 :b 2 :c 3)))


(let ((id 0))
  (defun next-id ()
    ;; wrap around eventually, so don't ask for them TOO often
    (when (> id 100000) (setf id 0))
    (s "x_~A" (incf id))))

;; (next-id)





(defun slurp-file (f &key (external-format :iso-8859-1) (element-type 'character))
  (with-open-file (s f
                     :element-type element-type
                     :if-does-not-exist nil :external-format external-format)
    (if s
        (slurp-stream s)
        nil)))

(defun slurp-stream (stream &key (element-type (stream-element-type stream)))
  (let* ((length (file-length stream))
         (seq (make-array length :element-type element-type :fill-pointer t)))
    ;; this is needed if the file length<>number of characters in file, which will happen for UTF-8 files etc
    (setf (fill-pointer seq)
          (read-sequence seq stream))
    seq))

;; write a string to a file.
(defun write-file (f data &key (external-format :iso-8859-1))
  (with-open-file (s f
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :external-format external-format
                     :element-type (array-element-type data))
    (write-sequence data s)))




;; These functions were done as part of Eve, but are quite useful

;; !!! It would be good to be able to have the time too
(defun yyyy-mm-dd-date (a &optional (zero-pad t))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time a)
    (declare (ignore hour minute second))
    (let ((x (format nil "~A-~A-~A" year month day)))
      (if zero-pad
          (regex-replace "-(\\d)$"
                         (regex-replace "-(\\d)-" x "-0\\1-")
                         "-0\\1")
          x))))

;; (yyyy-mm-dd-date (get-universal-time))

(defun hh-mm-ss-time (a &optional (zero-pad t))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time a)
    (declare (ignore day month year))
    (let ((x (format nil "~A-~A-~A" hour minute second)))
      (if zero-pad
          (regex-replace "-(\\d)$"
                         (regex-replace "-(\\d)-" x "-0\\1-")
                         "-0\\1")
          x))))

;; (hh-mm-ss-time (get-universal-time))



;; like in clojure I think
(defun str (&rest list)
  (with-output-to-string (stream)
    (dolist (x list)
      (format stream "~A" x))))



;; This gives the same output as the shell command I wrote
(defun now (&optional (time (get-universal-time)))
  (str (yyyy-mm-dd-date time)
       "---"
       (hh-mm-ss-time time)))


;; I think this should achieve what we need for timing things out without the central coordinator having to do it itself
;; that way we can structure everything with message passing
;; !!! This does not have access to dynamically bound variables
(defun do-with-timeout (timeout thunk &optional (error t))
  (let ((value nil)
        (error-result nil)
        (control (ccl:make-semaphore)))
    (let ((action (ccl:process-run-function "a"
                                            (lambda ()
                                              (handler-case
                                                  (setf value (funcall thunk))
                                                (t (c)
                                                  (setf error-result c)))
                                              (ccl:signal-semaphore control)))))
      (unless (ccl:timed-wait-on-semaphore control timeout)
        (ccl:process-kill action)
        (when error
          (error "Timed out!")))
      ;; We have to explicitly propagate the error out like this
      ;; because if there are error handlers in the thread where this is called from they
      ;; won't be available in the thread that is spawned
      (when error-result
        (error error-result))
      value)))


(defmethod sanitize-email-address ((x string))
  (unless (scan "\\@" x)
    (error "Invalid email address: ~A" x))
  (cl-ppcre:regex-replace-all "\\s" x ""))

;; (sanitize-email-address "foo@example.com ")
;; (sanitize-email-address "hi there ")


(defun merge-hash-tables (hash1 hash2 
                          &key (on-conflict #'+))
  "Merges hash table 2 into hash table 1.
If the key exists in both tables, on-conflict is called to resolve the value."
  (maphash 
   (lambda (key value2)  
     (let* ((value1 (gethash key hash1))
            (value (if value1
                       (funcall on-conflict value1 value2)
                       value2)))
       (setf (gethash key hash1)
             value)))
   hash2)

  hash1)
