(asdf:defsystem #:cl-imgmagick
  :version "0.0.1"
  :depends-on (:cffi :cl-ppcre :trivial-garbage)
  :components ((:file "packages")
               (:file "imgmagick-ffi" :depends-on ("packages"))
               (:file "imgmagick" :depends-on ("imgmagick-ffi"))))
