(in-package :imgdb-store)

(locally-enable-sql-reader-syntax)

(defparameter *img-types* '("jpeg" "jpg"))
(defparameter *default-dbconn-spec* '())
(defparameter *img-table-name* '[imgs])

(defun index-img-drop (img-drop img-store db-conn-spec db-type)
  "Indexes all images in img-drop by moving them to the img-store and creating an entry in the img-store database."
  (let ((num-imgs 0))
    (with-database (dbconn db-conn-spec :database-type db-type :pool t)
      (walk-directory
       img-drop
       (lambda (x)
         (handler-bind
             ((sql-database-data-error #'skip-img-record-creation))
           (when (index-img x img-store dbconn)
             (incf num-imgs))))
       :test
       (lambda (x)
         (and (file-exists-p x)
              (subsetp (list (pathname-type x)) *img-types*
                       :test #'string-equal)))))
    num-imgs))

(defun get-img-store-url (img-url year month day img-store)
  (merge-pathnames
   (make-pathname :name (pathname-name img-url)
                  :type (pathname-type img-url))
   (get-img-store-directory year month day img-store)))

(defun get-img-store-directory (year month day img-store)
  (merge-pathnames
   (make-pathname :directory
                  (if (and year month day)
                      (list :relative year month day)
                      (list :relative "undated")))
   img-store))

(let ((url-version-scanner (create-scanner "(.+)-(\\d+)$")))
  (defun get-next-available-img-store-url (img-store-url)
    (if (is-img-store-url-occupied img-store-url)
        (get-next-available-img-store-url
         (let* ((name (pathname-name img-store-url))
                (type (pathname-type img-store-url))
                (next-name
                 (register-groups-bind (name version)
                     (url-version-scanner name)
                   (unless (or (null name) (null version))
                     (let ((next-version (+ 1 (parse-integer version))))
                       (concatenate 'string
                                    name "-"
                                    (write-to-string next-version)))))))
           (merge-pathnames
            (make-pathname :name
                           (if next-name
                               next-name
                               (concatenate 'string name "-" "1"))
                           :type type)
            img-store-url)))
        img-store-url)))

(defun is-img-store-url-occupied (img-store-url)
  (probe-file img-store-url))

(defun move-img-from-img-drop-to-img-store (img-url year month day img-store)
  (let ((img-store-url
         (get-next-available-img-store-url
          (get-img-store-url img-url year month day img-store))))
    (ensure-directories-exist
     (directory-namestring img-store-url))
    (copy-file img-url img-store-url)
    img-store-url))

(defun skip-img-record-creation (c)
  (declare (ignore c))
  (let ((restart (find-restart 'skip-img-record-creation)))
    (if restart
        (invoke-restart restart)
        (error 'control-error))))

(defun index-img (img-url img-store dbconn)
  "Creates a record of the image in the database"
  ;; How can the insertion of duplicate file entries be handled?
  (let ((img-digest (byte-array-to-hex-string (digest-file :sha1 img-url))))
    (unless (img-record-exists img-digest dbconn)
      (with-magick-wand (wand)
        (magick-ping-image wand (namestring img-url))
        (let* ((img-date (or (image-original-date wand) (image-date wand)))
               (img-year (when img-date (first img-date)))
               (img-month (when img-date (second img-date)))
               (img-day (when img-date (third img-date)))
               (img-store-url
                (move-img-from-img-drop-to-img-store
                 img-url img-year img-month img-day img-store))
               (img-store-url-digest
                (byte-array-to-hex-string
                 (digest-sequence
                  :sha1
                  (ascii-string-to-byte-array (namestring img-store-url)))))
               (img-width (image-width wand))
               (img-height (image-height wand)))
          (restart-case
              (progn
                (insert-records :into *img-table-name*
                                :attributes
                                '([digest] [urldigest] [url]
                                  [width] [height]
                                  [year] [month] [day])
                                :values 
                                (list img-digest img-store-url-digest
                                      (namestring img-store-url)
                                      img-width img-height
                                      img-year img-month img-day)
                                :database dbconn)
                t)
            (skip-img-record-creation ()
              (delete-file img-store-url)
              nil)))))))

(defun remove-img-record (img-url dbconn)
  "Removes the record indexed by id from the database"
  (let ((img-urldigest (byte-array-to-hex-string
                        (digest-sequence
                         :sha1 (ascii-string-to-byte-array img-url)))))
    (delete-records :from *img-table-name*
                    :where [= [urldigest] img-urldigest]
                    :database dbconn)
    (delete-file img-url)))

(defmacro select-img-records (select-columns &rest args)
  `(select-from-table *img-table-name* ,select-columns ,@args))

(defun img-record-exists (img-digest dbconn)
  (> (caar (select [count [digest]] 
                   :from *img-table-name*
                   :where [= [digest] img-digest]
                   :database dbconn))
     0))

(defun count-img-records (dbconn)
  (caar (select [count [*]]
                :from *img-table-name*
                :database dbconn)))

(defun create-imgdb-dbconn (dbconn-spec db-type)
  "Connect to an imgdb database"
  (connect dbconn-spec :database-type db-type))
(defun destroy-imgdb-dbconn (dbconn)
  (disconnect :database dbconn))

(defun create-img-database (dbconn db-type)
  (create-database dbconn :database-type db-type))
(defun destroy-img-database (dbconn db-type)
  (destroy-database dbconn :database-type db-type))
(defun img-database-exists (dbconn db-type)
  (probe-database dbconn :database-type db-type))

(defun create-img-table (dbconn)
  (create-table *img-table-name*
                '(([digest] (vector char 40) :not-null :unique :primary-key)
                  ([urldigest] (vector char 40) :not-null)
                  ([url] string :not-null :unique)
                  ([width] integer :not-null)
                  ([height] integer :not-null)
                  ([year] integer)
                  ([month] integer)
                  ([day] integer))
                :database dbconn))
(defun drop-img-table (dbconn)
  (drop-table *img-table-name* :database dbconn))
(defun img-table-exists (dbconn)
  (table-exists-p *img-table-name* :database dbconn))

;;; Stubs
'(defun get-image (img-id dbconn))
'(defun repair-img-store (img-store dbconn))

(restore-sql-reader-syntax-state)
