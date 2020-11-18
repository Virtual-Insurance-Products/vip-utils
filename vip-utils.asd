
(asdf:defsystem :vip-utils
  :description "VIP Utilities - general macros and wotsits"
  :author "VIP"
  :serial t
  :depends-on (#:cl-ppcre #:anaphors #:ironclad #:cl-base64 #:trivial-utf-8)
  :components ((:file "package")
               (:file "functions")
               (:file "macros")
               (:file "more-functions")
               (:file "mop-utilities")
               (:file "conditions")
               (:file "types")
               (:file "names")))
