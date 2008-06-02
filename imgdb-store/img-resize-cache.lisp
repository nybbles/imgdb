(in-package :imgdb-store)

(locally-enable-sql-reader-syntax)

(define-condition cache-error () ())
(define-condition inconsistent-cache-error (cache-error) ())

(defmacro with-resized-image ((out img-id dimensions) &body body)
  (let ((dbconn-name (gensym "DBCONN-"))
        (url-name (gensym "URL-")))
    `(with-database (,dbconn-name *img-resize-cache-conn-spec*
                                  :database-type *img-resize-cache-conn-type*
                                  :pool t
                                  :if-exists :new)
       (unwind-protect
            (let ((,url-name
                   (acquire-resize-cache-entry ,img-id ,dimensions
                                               nil ,dbconn-name)))
              (with-open-file
                  (,out ,url-name
                        :element-type '(unsigned-byte 8)
                        :direction :input :if-does-not-exist :error)
                ,@body))
         (when (in-transaction-p :database ,dbconn-name)
           (rollback :database ,dbconn-name))
         (release-resize-cache-entry ,img-id ,dimensions nil ,dbconn-name)))))

(defmacro with-thumbnail ((out img-id size) &body body)
  (let ((dbconn-name (gensym "DBCONN-"))
        (url-name (gensym "URL-")))
    `(with-database (,dbconn-name *img-resize-cache-conn-spec*
                                  :database-type *img-resize-cache-conn-type*
                                  :pool t
                                  :if-exists :new)
       (unwind-protect
            (let ((,url-name
                   (acquire-resize-cache-entry ,img-id (list ,size ,size)
                                               t ,dbconn-name)))
              (with-open-file
                  (,out ,url-name
                        :element-type '(unsigned-byte 8)
                        :direction :input :if-does-not-exist :error)
                ,@body))
         (when (in-transaction-p :database ,dbconn-name)
           (rollback :database ,dbconn-name))
         (release-resize-cache-entry ,img-id (list ,size ,size)
                                     t ,dbconn-name)))))

(defmacro select-resize-cache-entries (select-columns &rest args)
  `(select-from-table *img-resize-cache-table* ,select-columns ,@args))

(defun get-original-image-url (img-id dbconn)
  (car
   (select-img-records ([url])
                       :where [= [digest] img-id]
                       :flatp t :database dbconn)))

(defun get-resize-cache-image-url (img-id dimensions thumbnail dbconn)
  (car (select-resize-cache-entries
        ([url])
        :where [and [= [originalimgid] img-id]
                    [= [width] (first dimensions)]
                    [= [height] (second dimensions)]
                    [= [thumbnail] (if thumbnail t "f")]]
        :flatp t
        :database dbconn)))

(defun generate-resize-cache-image-url
    (original-img-id dimensions thumbnail database)
  (namestring
   (let* ((width (write-to-string (first dimensions)))
          (height (write-to-string (second dimensions)))
          (original-img-url (get-original-image-url original-img-id database))
          (filename original-img-id)
          (filetype (pathname-type original-img-url)))
     (merge-pathnames
      (make-pathname :name (concatenate 'string filename "-" width "x" height)
                     :type filetype)
      (if thumbnail
          (merge-pathnames
           (make-pathname :directory (list :relative "thumbnails"))
           *img-resize-cache-store*)
          *img-resize-cache-store*)))))

(defun acquire-resize-cache-entry (img-id dimensions thumbnail dbconn)
  (ecase (database-type dbconn)
    (:postgresql (acquire-resize-cache-entry-postgresql
                  img-id dimensions thumbnail dbconn))))

(defun release-resize-cache-entry (img-id dimensions thumbnail dbconn)
  (remove-resize-cache-entry-hold img-id dimensions thumbnail dbconn))

(defun add-resize-cache-entry (img-id dimensions thumbnail url valid dbconn)
  (insert-records :into *img-resize-cache-table*
                  :attributes '(originalimgid url width height
                                thumbnail valid filesize)
                  :values
                  (list img-id url (first dimensions) (second dimensions)
                        (if thumbnail t "f") valid 0)
                  :database dbconn))

(defun remove-resize-cache-entry (img-id dimensions thumbnail dbconn)
  (delete-records :from *img-resize-cache-table*
                  :where [and [= [originalimgid] img-id]
                              [= [width] (first dimensions)]
                              [= [height] (second dimensions)]
                              [= [thumbnail] thumbnail]]
                  :database dbconn))

(defun get-resize-cache-entry-validity (img-id dimensions thumbnail dbconn)
  (let ((result (select-resize-cache-entries
                 ([valid])
                 :where [and [= [originalimgid] img-id]
                             [= [width] (first dimensions)]
                             [= [height] (second dimensions)]
                             [= [thumbnail] thumbnail]]
                 :flatp t
                 :database dbconn)))
    (assert (not (> (length result) 1)))
    result))

(defun set-resize-cache-entry-validity
    (img-id dimensions thumbnail valid dbconn)
  (update-records *img-resize-cache-table*
                  :attributes '(valid)
                  :values (list valid)
                  :where [and [= [originalimgid] img-id]
                              [= [width] (first dimensions)]
                              [= [height] (second dimensions)]
                              [= [thumbnail] thumbnail]]
                  :database dbconn))

(defun set-resize-cache-entry-filesize
    (img-id dimensions thumbnail filesize dbconn)
  (update-records *img-resize-cache-table*
                  :attributes '(filesize)
                  :values (list filesize)
                  :where [and [= [originalimgid] img-id]
                              [= [width] (first dimensions)]
                              [= [height] (second dimensions)]
                              [= [thumbnail] thumbnail]]
                  :database dbconn))

(defun add-resize-cache-entry-hold (img-id dimensions thumbnail dbconn)
  (insert-records :into *img-resize-cache-holds-table*
                  :attributes '(originalimgid width height
                                thumbnail threadid usetime)
                  :values (list img-id (first dimensions) (second dimensions)
                                (if thumbnail t "f")
                                (get-thread-id) (get-universal-time))
                  :database dbconn))

(defun remove-resize-cache-entry-hold (img-id dimensions thumbnail dbconn)
  (delete-records :from *img-resize-cache-holds-table*
                  :where [and [= [originalimgid] img-id]
                              [= [width] (first dimensions)]
                              [= [height] (second dimensions)]
                              [= [thumbnail] thumbnail]
                              [= [threadid] (get-thread-id)]]
                  :database dbconn))

(defparameter *select-unused-resize-cache-entries-query*
  "SELECT url, originalimgid, width, height, thumbnail, filesize
   FROM resizecache LEFT OUTER JOIN resizecacheholds
        ON (resizecache.originalimgid = resizecacheholds.originalimgid AND
            resizecache.width = resizecacheholds.width AND
            resizecache.height = resizecacheholds.height AND
            resizecache.thumbnail = resizecacheholds.thumbnail)
   WHERE resizecacheholds.originalimgid IS NULL
   FOR UPDATE")

(defun select-unused-resize-cache-entries (dbconn)
  (query *select-unused-resize-cache-entries-query* :database dbconn))

(defun flush-resize-cache (dbconn)
  (start-transaction :database dbconn)
  (let ((unused-resize-cache-entries
         (select-unused-resize-cache-entries dbconn))
        (result (list 0 0)))
    (dolist (i unused-resize-cache-entries result)
      (let ((url (first i))
            (img-id (second i))
            (width (third i))
            (height (fourth i))
            (thumbnail (fifth i))
            (filesize (sixth i)))
        (when (not (probe-file url))
          (error 'inconsistent-cache-error))
        (delete-file url)
        (remove-resize-cache-entry img-id (list width height) thumbnail dbconn)
        (incf (first result))
        (incf (second result) filesize))))
  (commit :database dbconn))

(defun resize-cache-entry-valid? (img-id dimensions thumbnail dbconn)
  (let ((result
         (select-resize-cache-entries
          ([valid])
          :where
          [and [= [originalimgid] img-id]
               [= [width] (first dimensions)]
               [= [height] (second dimensions)]
               [= [thumbnail] thumbnail]]
          :flatp t
          :database dbconn)))
    (assert (= (length result) 1))
    (first result)))

(defun resize-cache-full? (dbconn)
  (> (get-total-resize-cache-file-size dbconn) *img-resize-cache-max-size*))
(defun get-total-resize-cache-file-size (dbconn)
  (caar
   (select-resize-cache-entries
    ([sum [filesize]]) :database dbconn)))

(defun create-resize-cache-tables (dbconn)
  (create-table *img-resize-cache-table*
                '(([originalimgid] (vector char 40) :not-null)
                  ([width] integer :not-null)
                  ([height] integer :not-null)
                  ([thumbnail] boolean :not-null)
                  ([url] string :not-null :unique)
                  ([filesize] integer :not-null)
                  ([valid] boolean))
                :constraints
                '("PRIMARY KEY (originalimgid, width, height, thumbnail)")
                :database dbconn)
  (create-table *img-resize-cache-holds-table*
                '(([originalimgid] (vector char 40) :not-null)
                  ([width] integer :not-null)
                  ([height] integer :not-null)
                  ([thumbnail] boolean :not-null)
                  ([threadid] string :not-null)
                  ([usetime] bigint :not-null))
                :constraints
                '("PRIMARY KEY (originalimgid, width, height, thumbnail, threadid)")
                :database dbconn))

(defun drop-resize-cache-tables (dbconn)
  (drop-table *img-resize-cache-table* :database dbconn)
  (drop-table *img-resize-cache-holds-table* :database dbconn))
(defun resize-cache-tables-exist? (dbconn)
  (table-exists-p *img-resize-cache-table* :database dbconn)
  (table-exists-p *img-resize-cache-holds-table* :database dbconn))

(defun create-resized-image (img-id dimensions thumbnail dbconn)
  (let ((img-store-url
         (car (select-img-records ([url])
                                  :where [= [digest] img-id]
                                  :flatp t
                                  :database dbconn)))
        (resize-cache-url
         (get-resize-cache-image-url img-id dimensions thumbnail dbconn))
        (width (first dimensions))
        (height (second dimensions)))
    (assert (not (null img-store-url)))
    (assert (not (null resize-cache-url)))
    (if thumbnail
        (create-resized-image-thumbnail img-store-url resize-cache-url width)
        (create-resized-full-image img-store-url resize-cache-url
                                   width height))))

(defun create-resized-full-image
    (img-store-url resize-cache-url new-width new-height)
  (with-magick-wand (wand)
    (magick-read-image wand img-store-url)
    (magick-adaptive-resize-image wand new-width new-height)
    (with-image-blob (wand img-blob img-blob-size)
      (with-open-file
          (out resize-cache-url
               :element-type '(unsigned-byte 8)
               :direction :output :if-exists :error)
        (iterate-over-foreign-buffer
         (vec 65536 vec-pos) (img-blob img-blob-size img-blob-pos)
         (write-sequence vec out :end vec-pos))))))

(defun create-resized-image-thumbnail
    (img-store-url resize-cache-url new-size)
  (with-magick-wand (wand)
    (magick-read-image wand img-store-url)
    (let* ((width (image-width wand))
           (height (image-height wand))
           (size (min width height))
           (x (floor (/ (- width size) 2)))
           (y (floor (/ (- height size) 2))))
      (magick-crop-image wand size size  x y)
      (magick-thumbnail-image wand new-size new-size)
      (with-image-blob (wand img-blob img-blob-size)
        (with-open-file
            (out resize-cache-url
                 :element-type '(unsigned-byte 8)
                 :direction :output :if-exists :error)
          (iterate-over-foreign-buffer
           (vec 65536 vec-pos) (img-blob img-blob-size img-blob-pos)
           (write-sequence vec out :end vec-pos)))))))

(restore-sql-reader-syntax-state)
