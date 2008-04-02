(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun img-url-handler ()
  (let* ((img-id (get-img-id-from-uri (script-name)))
         (req-params (get-parameters))
         (req-type (read-from-string (caar req-params)))
         (req-arg (parse-integer (cdar req-params) :junk-allowed t)))
    (if (and (= (length req-params) 1)
             (not (null req-arg))
             (subsetp (list req-type)
                      '(resize width height)))
        ;; Check whether image exists in imgdb and get image
        ;; location in imgdb-store.
        (let ((imgdb-store-url (get-imgdb-store-url-from-img-id img-id)))
          (if (null imgdb-store-url)
              (not-found-page)
              (progn
                ;; Read image and get dimensions.
                ;; Get resized dimensions.
                ;; Resize the image, if needed.
                ;; Send the (resized?) image back.
                ;; It doesn't look like there is any way in MagickWand
                ;; of doing the reading, resizing and sending of the
                ;; image using buffers (instead of loading the entire
                ;; image into memory).
                (format nil "still under construction"))))
        (not-found-page))))

(defun get-resized-dimensions (width height req-type req-arg)
  (ccase req-type
    (resize
     (get-resized-dimensions-for-percentage-resize width height req-arg))
    (width
     (get-resized-dimensions-for-width-resize width height req-arg))
    (height
     (get-resized-dimensions-for-height-resize width height req-arg))))

(defun get-resized-dimensions-for-percentage-resize (width height percent)
  (mapcar #'(lambda (x)
              (floor (* x (/ percent 100))))
          (list width height)))

(defun get-resized-dimensions-for-width-resize (width height new-width)
  (list new-width (floor (* height (/ new-width width)))))

(defun get-resized-dimensions-for-height-resize (width height new-height)
  (list (floor (* width (/ new-height height))) new-height))

(defun get-imgdb-store-url-from-img-id (img-id)
  (caar (select-img-records ([url]) [= [digest] img-id] *imgdb-dbconn*)))

(defun get-img-id-from-uri (uri)
  (register-groups-bind (img-id)
      ("/img-urls/([0-9a-z]*)(\\?|$)" uri)
    img-id))

(push (create-regex-dispatcher "/img-urls/.*" 'img-url-handler)
      *dispatch-table*)

(restore-sql-reader-syntax-state)
