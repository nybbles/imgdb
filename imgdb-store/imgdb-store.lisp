(in-package :imgdb-store)

(defparameter *img-types* '("jpeg" "jpg"))
(defparameter *default-dbconn-spec* '())
(defparameter *img-table-name* '[imgs])

(defun index-directory (imgdrop imgstore dbconn)
  "Indexes all images in imgdrop by moving them to the imgstore and creating an entry in the imgstore database."
  (let ((numimgs 0))
    (walk-directory
     imgdrop
     (lambda (x)
       (incf numimgs)
       (handler-bind ((sql-database-data-error #'skip-img-record-creation))
         (create-img-record x dbconn)))
     :test
     (lambda (x)
       (and (file-exists-p x)
            (subsetp (list (pathname-type x)) *img-types*
                     :test #'string-equal))))
    numimgs))

(defun skip-img-record-creation (c)
  (declare (ignore c))
  (let ((restart (find-restart 'skip-img-record-creation)))
    (if restart
        (invoke-restart restart)
        (error 'control-error))))

(defun create-img-record (img-url dbconn)
  "Creates a record of the image in the database"
  ;; How can the insertion of duplicate file entries be handled?
  (let* ((img-digest (byte-array-to-hex-string (digest-file :md5 img-url)))
         (img-url (concatenate 'string "file://" (namestring img-url)))
         (img-urldigest (byte-array-to-hex-string 
                     (digest-sequence
                      :md5 (ascii-string-to-byte-array img-url)))))
    (restart-case
        (insert-records :into *img-table-name*
                        :attributes '([digest] [urldigest] [url])
                        :values 
                        (list img-digest img-urldigest img-url)
                        :database dbconn)
      (skip-img-record-creation () nil))))

(defun remove-img-record (img-url dbconn)
  "Removes the record indexed by id from the database"
  (let ((img-urldigest (byte-array-to-hex-string
                        (digest-sequence
                         :md5 (ascii-string-to-byte-array img-url)))))
    (delete-records :from *img-table-name*
                    :where [= [urldigest] img-urldigest]
                    :database dbconn)))

(defun connect-to-dbserver (dbconn-spec db-type)
  "Connect to a database"
  (connect dbconn-spec :database-type db-type))

(defun disconnect-from-dbserver (dbconn)
  (disconnect :database dbconn))

(defun create-img-database (dbconn db-type)
  (create-database dbconn :database-type db-type))
(defun destroy-img-database (dbconn db-type)
  (destroy-database dbconn :database-type db-type))
(defun img-database-exists (dbconn db-type))

(defun create-img-table (dbconn)
  (create-table *img-table-name*
                '(([digest] (vector char 32) :not-null)
                  ([urldigest] (vector char 32) :not-null :unique :primary-key)
                  ([url] string :not-null :unique))
                :database dbconn))
(defun drop-img-table (dbconn)
  (drop-table *img-table-name* :database dbconn))
(defun img-table-exists (dbconn))
