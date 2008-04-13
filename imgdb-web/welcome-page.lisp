(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun welcome-page ()
  (let ((random-img-url
         (concatenate 'string
                      "img-urls/" (select-random-img-id) "?height=500")))
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
              (:img :height 500 :src (str random-img-url)))
        (:div :id "date-cloud"
              (:h3 :align "center" "Pictures by year")
              (str (generate-tag-cloud
                    (convert-num-imgs-by-year-results-to-tag-list
                     (select-num-imgs-by-year))))))))))

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
  (concatenate 'string "img-query" "?year=" tag-name))

(defun select-num-imgs-by-year ()
  (select-img-records ([year] [count [*]])
                      :group-by [year]))

(defun select-random-img-id ()
  (caar (select-img-records ([digest])
                           :order-by (sql-operation 'function "random" 42)
                           :limit 1 :database *imgdb-dbconn*)))

(push (create-prefix-dispatcher "/index.htm" 'welcome-page) *dispatch-table*)
(push (create-regex-dispatcher "^/$" 'welcome-page) *dispatch-table*)

(restore-sql-reader-syntax-state)
