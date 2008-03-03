(asdf:defsystem #:imgdb-store
  :version "0.0.1"
  :depends-on (:cl-fad :ironclad :clsql :cl-imgmagick :cl-ppcre)
  :components ((:file "packages")
               (:file "imgdb-store" :depends-on ("packages"))))
