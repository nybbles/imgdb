(in-package :imgdb-web)

(define-condition invalid-img-query-error () ())

(defun img-query-page ()
  (handler-case
      (let* ((params (get-parameters))
             (constraints (translate-get-parameters-to-constraints params))
             (current (get-current-constraint-value constraints)))
        (with-database (dbconn *imgdb-store-db-conn-spec*
                               :database-type *imgdb-store-db-type*
                               :pool t :if-exists :old)
          (with-html-output-to-string (output nil :prologue t)
            (:html
             (str (generate-html-head "imgdb"))
             (:body
              (:div :id "signin"
                    (:a :href "login.htm" "Sign in"))
              (:br)
              (:br)
              (:div :id "front-content"
                    (:h1 :align "center" "imgdb")
                    (str
                     (create-thumbnail-grid
                      (get-img-ids current 40
                                   (translate-constraints-to-sql constraints)
                                   dbconn)))
                    (:br)
                    (str
                     (create-links-for-query-resultset
                      constraints current 40 dbconn)))
              (:div :id "date-cloud"
                    (str (generate-date-cloud constraints dbconn))))))))
    (invalid-img-query-error () (not-found-page))))
