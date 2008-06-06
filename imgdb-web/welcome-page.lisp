(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun welcome-page ()
  (img-query-page))

(defun select-random-img-id (dbconn)
  (caar (select-img-records (list [digest])
                           :order-by (sql-operation 'function "random" 42)
                           :limit 1 :database dbconn)))

(restore-sql-reader-syntax-state)
