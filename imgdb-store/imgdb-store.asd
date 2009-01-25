(asdf:defsystem #:imgdb-store
  :version "0.0.1"
  :depends-on (:cl-fad :ironclad :clsql :cl-ppcre :hunchentoot
               :cl-imgmagick :ffi-utils)
  :components ((:file "packages")
               (:file "imgdb-store-utils" :depends-on ("packages"))
               (:file "imgdb-store" :depends-on ("imgdb-store-utils"))
               (:file "imgdb-store-tables"
                      :depends-on
                      ("imgdb-store" "imgdb-tags" "img-resize-cache"))
               (:file "img-resize-cache-postgresql"
                      :depends-on ("imgdb-store-utils"))
               (:file "img-resize-cache-sqlite"
                      :depends-on ("imgdb-store-utils"))
               (:file "img-resize-cache"
                      :depends-on ("imgdb-store"
                                   "img-resize-cache-postgresql"
                                   "img-resize-cache-sqlite"))
               (:file "imgdb-tags"
                      :depends-on ("imgdb-store"))))
