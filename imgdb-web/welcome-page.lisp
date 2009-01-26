(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defmethod welcome-page ((web imgdb-web-server))
  (img-query-page web))

(defun select-random-img-id (dbconn)
  (caar (select-img-records (list [digest])
                           :order-by (sql-operation 'function "random" 42)
                           :limit 1 :database dbconn)))

(restore-sql-reader-syntax-state)
