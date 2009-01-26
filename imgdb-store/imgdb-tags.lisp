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

(defun select-num-imgs-for-tag-type
    (tag-type constraints &key database (order :desc))
  (let* ((tag-type-column (sql-expression
                           :attribute (intern (string-upcase tag-type))))
         (select-columns (list tag-type-column [count [*]]))
         (where-clause (translate-constraints-to-sql constraints))
         (order-by-clause (list (list tag-type-column order)))
         (result (select-img-records select-columns
                                     :where where-clause
                                     :group-by tag-type-column
                                     :order-by order-by-clause
                                     :database database))
         (last-tag (car (last result))))
    ;; TODO (NM): Find out what this code is doing and document it.
    (if (and (eq order :asc) (not (null result)) (null (car last-tag)))
        (push last-tag (subseq result 0 (- (length result) 1)))
        result)))

(restore-sql-reader-syntax-state)
