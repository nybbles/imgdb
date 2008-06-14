(asdf:defsystem #:imgdb-web
  :version "0.0.1"
  :depends-on (:hunchentoot :flexi-streams :cl-who :cl-ppcre
               :cl-imgmagick :imgdb-store :ffi-utils)
  :components
  ((:file "packages")
   (:file "imgdb-web-setup" :depends-on ("packages"))
   (:file "xhtml-gen-utils" :depends-on ("imgdb-web-setup"))
   (:file "img-urls" :depends-on ("imgdb-web-setup" "not-found-page"))
   (:file "not-found-page" :depends-on ("xhtml-gen-utils"))
   (:file "welcome-page" :depends-on ("xhtml-gen-utils" "date-cloud"))
   (:file "tag-cloud" :depends-on ("packages"))
   (:file "date-cloud" :depends-on ("tag-cloud"))
   (:file "img-query-constraint" :depends-on ("packages"))
   (:file "img-thumbnail-grid" :depends-on ("packages" "img-query-constraint"))
   (:file "imgdb-web-server" :depends-on ("imgdb-web-setup"))
   (:file "img-query" :depends-on
          ("xhtml-gen-utils" "date-cloud" "img-query-constraint"))
   (:file "img-view" :depends-on
          ("xhtml-gen-utils" "tag-cloud" "img-urls"))
   (:file "dispatchers-setup"
          :depends-on ("not-found-page" "img-urls" "welcome-page"))))
