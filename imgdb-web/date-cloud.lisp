(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun generate-date-cloud (constraints))

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

(defun select-num-imgs-by-year (dbconn)
  (select-img-records ([year] [count [*]])
                      :group-by [year]
                      :order-by '(([year] :desc))
                      :database dbconn))

(restore-sql-reader-syntax-state)
