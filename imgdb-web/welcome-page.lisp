(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun welcome-page ()
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
              (get-img-ids 1 10
                           (translate-constraints-to-sql '())
                           *imgdb-dbconn*)))
            (:br)
            (str
             (create-links-for-query-resultset '() 1 10)))
      (:div :id "date-cloud"
            (:h3 :align "center" "Pictures by year")
            (str (generate-tag-cloud
                  (convert-num-imgs-by-year-results-to-tag-list
                   (select-num-imgs-by-year *imgdb-dbconn*)))))))))

(defun convert-num-imgs-by-year-results-to-tag-list (query-results)
  (mapcar
   #'(lambda (x)
       (let* ((year (first x))
              (year-str (if (null year) "undated" (write-to-string year)))
              (link (get-date-cloud-tag-link year-str))
              (quantity (second x)))
         (list year-str link quantity)))
   query-results))

(defun get-date-cloud-tag-link (tag-name)
  (concatenate 'string "img-query" "?current=1&year=" tag-name))

(defun select-num-imgs-by-year (database)
  (select-img-records ([year] [count [*]])
                      :group-by [year]
                      :order-by '(([year] :desc))
                      :database database))

(defun select-random-img-id (database)
  (caar (select-img-records ([digest])
                           :order-by (sql-operation 'function "random" 42)
                           :limit 1 :database database)))

(restore-sql-reader-syntax-state)
