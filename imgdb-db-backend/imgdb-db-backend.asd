(asdf:defsystem #:imgdb-db-backend
  :version "0.0.1"
  :depends-on (:cl-fad :ironclad :clsql)
  :components ((:file "packages")
               (:file "imgdb-db-backend" :depends-on ("packages"))))
