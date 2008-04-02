(asdf:defsystem #:imgdb-web
  :version "0.0.1"
  :depends-on (:hunchentoot :cl-who :cl-ppcre :cl-imgmagick :imgdb-store)
  :components ((:file "packages")
               (:file "imgdb-web-setup" :depends-on ("packages"))
               (:file "xhtml-gen-utils" :depends-on ("imgdb-web-setup"))
               (:file "imgdb-web-css" :depends-on ("imgdb-web-setup"))
               (:file "img-urls"
                      :depends-on ("imgdb-web-setup" "not-found-page"))
               (:file "not-found-page" :depends-on ("xhtml-gen-utils"))
               (:file "welcome-page" :depends-on ("xhtml-gen-utils"))
               (:file "imgdb-web-server" :depends-on ("packages"))))
