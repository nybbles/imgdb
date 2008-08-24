(asdf:defsystem #:imgdb-tests
  :version "0.0.1"
  :depends-on (:fiveam)
  :components
  ((:file "packages")
   (:file "imgdb-store-test" :depends-on ("packages"))))
