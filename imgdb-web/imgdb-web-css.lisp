(in-package :imgdb-web)

(defun imgdb-web-css ()
  (if (null *imgdb-web-root*)
      (error "imgdb-web's root is not defined")
      (handle-static-file
       (merge-pathnames "css/imgdb-web.css" *imgdb-web-root*))))

