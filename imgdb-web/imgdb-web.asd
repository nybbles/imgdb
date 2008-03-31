(asdf:defsystem #:imgdb-web
  :version "0.0.1"
  :depends-on (:hunchentoot :cl-who)
  :components ((:file "packages")
               (:file "imgdb-web-setup" :depends-on ("packages"))
               (:file "imgdb-web-css" :depends-on ("imgdb-web-setup"))
               (:file "welcome-page" :depends-on ("imgdb-web-setup"))))
