(in-package :imgdb-store)

(locally-enable-sql-reader-syntax)

(defparameter *lock-cache-entry-for-read-postgresql-query*
  (concatenate 'string
               "SELECT valid FROM " (sql *img-resize-cache-table*)
               " WHERE (originalimgid = \'~A\' AND"
               "        width = ~D AND height = ~D AND"
               "        thumbnail = \'~A\') FOR SHARE"))

(defun lock-cache-entry-for-read-postgresql (img-id dimensions thumbnail dbconn)
  (car (query (format nil *lock-cache-entry-for-read-postgresql-query*
                      img-id (first dimensions) (second dimensions)
                      (if thumbnail "TRUE" "FALSE"))
              :database dbconn)))

(defparameter *lock-cache-entry-for-write-postgresql-query*
  (concatenate 'string
               "SELECT valid FROM " (sql *img-resize-cache-table*)
               " WHERE (originalimgid = \'~A\' AND"
               "        width = ~D AND height = ~D AND"
               "        thumbnail = \'~A\') FOR UPDATE"))

(defun lock-cache-entry-for-write-postgresql
    (img-id dimensions thumbnail dbconn)
  (car (query (format nil *lock-cache-entry-for-read-postgresql-query*
                      img-id (first dimensions) (second dimensions)
                      (if thumbnail "TRUE" "FALSE"))
              :database dbconn)))

(defun lock-cache-table-for-write-postgresql (dbconn)
  (caar (query "LOCK TABLE resizecache IN ACCESS EXCLUSIVE MODE"
               :database dbconn)))

(defun acquire-resize-cache-entry-postgresql
    (img-id dimensions thumbnail dbconn)
  (assert (eq (database-type dbconn) :postgresql))
  (add-resize-cache-entry-hold img-id dimensions dbconn)
  (start-transaction :database dbconn)
  (let*
      ((found (lock-cache-entry-for-read-postgresql
               img-id dimensions thumbnail dbconn))
       (valid (first found)))
    (unless found
      (lock-cache-table-for-write-postgresql dbconn)
      (setf found
            (lock-cache-entry-for-write-postgresql
             img-id dimensions thumbnail dbconn))
      (setf valid (first found))
      (unless found
        (let ((resized-image-url
               (generate-resize-cache-image-url
                img-id dimensions thumbnail dbconn)))
          (add-resize-cache-entry img-id dimensions resized-image-url nil))
        (commit :database dbconn)
        (setf valid nil))
      (when (in-transaction-p :database dbconn)
        (rollback :database dbconn))
      (start-transaction :database dbconn))
    (unless valid
      (let ((found
             (lock-cache-entry-for-write-postgresql
              img-id dimensions thumbnail dbconn)))
        (assert found)
        (setf valid (first found))
        (unless valid
          (create-resized-image img-id dimensions thumbnail dbconn)
          (set-resize-cache-entry-validity img-id dimensions thumbnail t dbconn)
          (commit :database dbconn)))))
  (let ((resized-image-url
         (get-resize-cache-image-url img-id dimensions thumbnail dbconn)))
    (when (in-transaction-p :database dbconn)
      (rollback :database dbconn))
    (assert (not (null resized-image-url)))
    resized-image-url))

(restore-sql-reader-syntax-state)
