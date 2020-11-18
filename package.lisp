
(in-package :cl-user)


(defpackage :vip-utils
  (:use :cl :cl-ppcre :anaphors)
  (:export
   #:defun/c
   #:distinct
   #:hash-keys
   #:hash
   #:find-all    ; just a thin wrapper around remove with test-not
   #:make-same-length

   #:*defun-memo*
   #:defun/m
   #:clear-function-memo
   #:defun/m*

   #:@
   #:@@
   
   #:bind

   #:seq
   #:compose
   #:defun*
   #:cps-seq
   #:cps-seq-transform ; used by monads.asd

   #:filter ; !!! REMOVE ME
   #:string-list
   #:s
   #:random-string
   #:group
   #:strip-leading-and-trailing-space

   #:parse-date
   #:parse-datetime

   ;; threading macros
   #:~>>
   #:~>
   #:as~>

   #:formatted-date
   #:formatted-time
   #:formatted-date-time
   #:parse-rational
   #:is-leap-year 

   #:zero-pad
   #:trim-string
   #:large-decimal-expansion

   #:sensible-round

   #:round-price-to-nearest-penny
   
   #:shallow-copy-object
   #:update-slots

   #:run-program
   #:shell

   #:flatten

   #:hash #:hash-values #:hash-values-and-keys

   #:next-id

   #:slurp-file
   #:slurp-stream
   #:write-file

   
   #:yyyy-mm-dd-date
   #:hh-mm-ss-time

   ;; like in clojure I think
   #:str
   #:now

   #:make-programmatic-instance
   #:allocate-programmatic-instance

   #:do-with-timeout
   
   #:ignorable-destructuring-bind
   #:invalid-form-value
   #:merge-sort
   #:casep
   
   #:barf
   
   #:string-to-class-name 
   #:class-name-to-string
   
   #:split-lines
   #:string-empty-p
   
   #:collapse-string-alist

   #:sanitize-email-address

   #:call-next-method?

   #:with-partially-applied

   #:password-pwned-p

   #:merge-hash-tables 
   ;; Types
   ;; #:tuple ; causes name conflicts

   #:apply-to-object

   #:ensure-type
   #:tlet
   #:tlet*

   #:get-random-name
   ))
