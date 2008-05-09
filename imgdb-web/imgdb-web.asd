(asdf:defsystem #:imgdb-web
  :version "0.0.1"
  :depends-on (:hunchentoot :cl-who :cl-ppcre
               :cl-imgmagick :imgdb-store :ffi-utils)
  :components
  ((:file "packages")
   (:file "imgdb-web-setup" :depends-on ("packages"))
   (:file "xhtml-gen-utils" :depends-on ("imgdb-web-setup"))
   (:file "imgdb-web-css" :depends-on ("imgdb-web-setup"))
   (:file "img-urls" :depends-on ("imgdb-web-setup" "not-found-page"))
   (:file "not-found-page" :depends-on ("xhtml-gen-utils"))
   (:file "welcome-page" :depends-on ("xhtml-gen-utils" "tag-cloud"))
   (:file "tag-cloud" :depends-on ("packages"))
   (:file "img-thumbnail-grid" :depends-on ("packages"))
   (:file "imgdb-web-server" :depends-on ("imgdb-web-setup"))
   (:file "dispatchers-setup"
          :depends-on
          ("imgdb-web-css" "not-found-page" "img-urls" "welcome-page"))))
