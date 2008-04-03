(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun img-url-handler ()
  (let* ((img-id (get-img-id-from-uri (script-name)))
         (req-params (get-parameters))
         (req-type (when req-params (read-from-string (caar req-params))))
         (req-arg (when req-params
                    (parse-integer (cdar req-params) :junk-allowed t))))
    (cond 
        ((and (= (length req-params) 1)
              (not (null req-arg))
              (subsetp (list req-type)
                       '(resize width height)))
         (let ((img-store-url-and-size
                (get-img-store-url-and-size-from-img-id img-id)))
           (if (null img-store-url-and-size)
               (not-found-page)
               (let* ((img-store-url (first img-store-url-and-size))
                      (img-width (second img-store-url-and-size))
                      (img-height (third img-store-url-and-size))
                      (resized-dimensions
                       (resize-required? img-width img-height
                                         req-type req-arg)))
                 (if (not resized-dimensions)
                     (transfer-unresized-image img-store-url)
                     (transfer-resized-image img-store-url
                                             (first resized-dimensions)
                                             (second resized-dimensions)))))))
      ((= (length req-params) 0)
       (let ((img-store-url
              (first (get-img-store-url-and-size-from-img-id img-id))))
         (transfer-unresized-image img-store-url)))
      (t (not-found-page)))))

(defun transfer-unresized-image (img-store-url)
  ;; Just serve up the unaltered image
  (format nil "still under construction"))
(defun transfer-resized-image (img-store-url new-width new-height)
  ;; Read and resize the image.
  ;; Resize the image
  ;; Send the resized image back.
  ;; It doesn't look like there is any way in MagickWand
  ;; of doing the reading, resizing and sending of the
  ;; image using buffers (instead of loading the entire
  ;; image into memory).
  (format nil "still under construction"))

(defun resize-required? (width height req-type req-arg)
  (ccase req-type
    (resize
     (and (not (= req-arg 100))
          (resize-dimensions-by-percentage width height req-arg)))
    (width
     (and (not (= req-arg width))
          (resize-dimensions-to-width width height req-arg)))
    (height
     (and (not (= req-arg height))
          (resize-dimensions-to-height width height req-arg)))))

(defun resize-dimensions-by-percentage (width height percent)
  (mapcar #'(lambda (x)
              (floor (* x (/ percent 100))))
          (list width height)))

(defun resize-dimensions-to-width (width height new-width)
  (list new-width (floor (* height (/ new-width width)))))

(defun resize-dimensions-to-height (width height new-height)
  (list (floor (* width (/ new-height height))) new-height))

(defun get-img-store-url-and-size-from-img-id (img-id)
  (car (select-img-records ([url] [width] [height])
                           [= [digest] img-id] *imgdb-dbconn*)))

(defun get-img-id-from-uri (uri)
  (register-groups-bind (img-id)
      ("/img-urls/([0-9a-z]*)(\\?|$)" uri)
    img-id))

(push (create-regex-dispatcher "/img-urls/.*" 'img-url-handler)
      *dispatch-table*)

(restore-sql-reader-syntax-state)
