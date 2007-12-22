(asdf:defsystem #:img-io
  :version "0.0.1"
  :depends-on (:cffi)
  :components ((:file "packages")
               (:file "img-io" :depends-on ("packages"))))
