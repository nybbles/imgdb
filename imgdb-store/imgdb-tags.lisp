(in-package :imgdb-store)

(locally-enable-sql-reader-syntax)

(defparameter *max-tag-length* 64)

(defun get-img-tags (img-id dbconn)
  (select [tag]
          :from *img-tags-table*
          :flatp t
          :where [= [imgid] img-id]
          :database dbconn))

(defun add-img-tags (img-id tags dbconn)
  (with-transaction (:database dbconn)
    (let ((tags-to-add (set-difference tags (get-img-tags img-id dbconn))))
      (dolist (tag tags-to-add)
        (insert-records :into *img-tags-table*
                        :attributes '(imgid tag)
                        :values (list img-id tag)
                        :database dbconn)))
    (get-img-tags img-id dbconn)))

(defun delete-img-tags (img-id tags dbconn)
  (with-transaction (:database dbconn)
    (delete-records :from *img-tags-table*
                    :where
                    [and [= [imgid] img-id] [in [tag] tags]]
                    :database dbconn)
    (get-img-tags img-id dbconn)))

(defun create-img-tags-table (dbconn-spec db-type)
  (with-database (dbconn dbconn-spec
                         :database-type db-type :pool t :if-exists :old)
    (create-table *img-tags-table*
                  '(([imgid] (vector char 40) :not-null)
                    ([tag] (vector char *max-tag-length*) :not-null))
                  :constraints '("PRIMARY KEY (imgid, tag)")
                  :database dbconn)))
(defun drop-img-tags-table (dbconn-spec db-type)
  (with-database (dbconn dbconn-spec
                         :database-type db-type :pool t :if-exists :old)
    (drop-table *img-tags-table* :database dbconn)))
(defun img-tags-table-exists (dbconn-spec db-type)
  (with-database (dbconn dbconn-spec
                         :database-type db-type :pool t :if-exists :old)
    (table-exists-p *img-tags-table* :database dbconn)))

(restore-sql-reader-syntax-state)
