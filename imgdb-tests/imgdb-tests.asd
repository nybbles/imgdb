(asdf:defsystem #:imgdb-tests
  :version "0.0.1"
  :depends-on (:fiveam :imgdb-store :imgdb-web)
  :components
  ((:file "packages")
   (:file "imgdb-store-test" :depends-on ("packages"))
   (:file "imgdb-web-test" :depends-on ("packages"))))
