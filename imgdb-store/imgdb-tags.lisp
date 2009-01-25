(in-package :imgdb-store)

(locally-enable-sql-reader-syntax)

(defparameter *max-tag-length* 64)

(defmethod get-img-tags ((store imgdb-store) img-id dbconn)
  (select [tag]
          :from *img-tags-table*
          :flatp t
          :where [= [imgid] img-id]
          :database dbconn))

(defmethod add-img-tags ((store imgdb-store) img-id tags dbconn)
  (with-transaction (:database dbconn)
    (let ((tags-to-add (set-difference tags (get-img-tags img-id dbconn))))
      (dolist (tag tags-to-add)
        (insert-records :into *img-tags-table*
                        :attributes '(imgid tag)
                        :values (list img-id tag)
                        :database dbconn)))
    (get-img-tags img-id dbconn)))

(defmethod delete-img-tags ((store imgdb-store) img-id tags dbconn)
  (with-transaction (:database dbconn)
    (delete-records :from *img-tags-table*
                    :where
                    [and [= [imgid] img-id] [in [tag] tags]]
                    :database dbconn)
    (get-img-tags img-id dbconn)))

(defmethod create-img-tags-table ((store imgdb-store) dbconn)
  (create-table *img-tags-table*
                '(([imgid] (vector char 40) :not-null)
                  ([tag] (vector char *max-tag-length*) :not-null))
                :constraints '("PRIMARY KEY (imgid, tag)")
                :database dbconn))

(defmethod drop-img-tags-table ((store imgdb-store) dbconn)
  (drop-table *img-tags-table* :database dbconn))

(defmethod img-tags-table-exists ((store imgdb-store) dbconn)
  (table-exists-p *img-tags-table* :database dbconn))

(restore-sql-reader-syntax-state)
