(in-package :imgdb-store)

(locally-enable-sql-reader-syntax)

;;;; Database table creation/deletion functions
(defmethod create-img-table ((store imgdb-store) dbconn)
  (create-table *img-table*
                '(([digest] (vector char 40) :not-null :unique :primary-key)
                  ([urldigest] (vector char 40) :not-null)
                  ([url] string :not-null :unique)
                  ([width] integer :not-null)
                  ([height] integer :not-null)
                  ([year] integer)
                  ([month] integer)
                  ([day] integer)
                  ([title] (vector char 40))
                  ([description] blob))
                :database dbconn))

(defmethod drop-img-table ((store imgdb-store) dbconn)
  (drop-table *img-table* :database dbconn))

(defmethod img-table-exists ((store imgdb-store) dbconn)
  (table-exists-p *img-table* :database dbconn))

(defmethod create-all-tables ((store imgdb-store) dbconn)
  (unless (img-table-exists store dbconn)
    (create-img-table store dbconn))
  (unless (img-tags-table-exists store dbconn)
    (create-img-tags-table store dbconn))
  (unless (resize-cache-tables-exist? store dbconn)
    (create-resize-cache-tables store dbconn)))

(defmethod drop-all-tables ((store imgdb-store) dbconn)
  (drop-img-table store dbconn)
  (drop-img-tags-table store dbconn)
  (drop-resize-cache-tables store dbconn))

(restore-sql-reader-syntax-state)
