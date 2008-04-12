(asdf:defsystem #:ffi-utils
  :version "0.0.1"
  :depends-on (:cffi)
  :components ((:file "packages")
               (:file "foreign-buffer" :depends-on ("packages"))))
