(asdf:defsystem #:imgdb-web
  :version "0.0.1"
  :depends-on (:hunchentoot :cl-who)
  :components ((:file "packages")
               (:file "imgdb-web-setup" :depends-on ("packages"))
               (:file "xhtml-gen-utils" :depends-on ("imgdb-web-setup"))
               (:file "imgdb-web-css" :depends-on ("imgdb-web-setup"))
               (:file "not-found-page" :depends-on ("xhtml-gen-utils"))
               (:file "welcome-page" :depends-on ("xhtml-gen-utils"))))
