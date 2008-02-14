(asdf:defsystem #:cl-imgmagick
  :version "0.0.1"
  :depends-on (:cffi)
  :components ((:file "packages")
               (:file "imgmagick-ffi" :depends-on ("packages"))))
