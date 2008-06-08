(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun img-view-page ()
  (let ((img-id (cdr (assoc "id" (get-parameters) :test #'equal))))
    (with-database (dbconn *imgdb-store-db-conn-spec*
                           :database-type *imgdb-store-db-type*
                           :pool t :if-exists :old)
      (with-html-output-to-string (output nil :prologue t)
        (:html
         (str (generate-html-head (format nil "imgdb - ~A" img-id)))
         (:body
          (:div :id "img-view-image"
                (:a :href (format nil "/img-urls/~A" img-id)
                    (:img :src (format nil "/img-urls/~A?width=500" img-id))))
          (:br)
          (:div
           :id "signin"
           (:h4
            (str
             (format nil "Taken on: ~A"
                     (translate-date-to-date-str
                      (select-img-date img-id dbconn) :type :long)))))))))))

(defun select-img-date (img-id dbconn)
  (car (select-img-records (list [year] [month] [day])
                           :where [= [digest] img-id]
                           :flatp t
                           :database dbconn)))

(restore-sql-reader-syntax-state)
