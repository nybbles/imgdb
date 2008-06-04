(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun welcome-page ()
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
              (:p
               "It's still under construction, so everything looks terrible.")
              (str
               (create-thumbnail-grid
                (get-img-ids 1 40
                             (translate-constraints-to-sql '())
                             dbconn)))
              (:br)
              (str
               (create-links-for-query-resultset '() 1 40 dbconn)))
        (:div :id "date-cloud"
              (:h3 :align "center" "Pictures by year")
              (str (generate-tag-cloud
                    (convert-num-imgs-by-year-results-to-tag-list
                     (select-num-imgs-by-year dbconn))))))))))

(defun select-random-img-id (dbconn)
  (caar (select-img-records ([digest])
                           :order-by (sql-operation 'function "random" 42)
                           :limit 1 :database dbconn)))

(restore-sql-reader-syntax-state)
