(in-package :imgdb-web)

(defun img-url-handler ()
  (let ((img-id (get-img-id-from-uri (script-name)))
        (req-params (get-parameters)))
    (if (and (= (length req-params) 1)
             (subsetp (list (caar req-params))
                      '("resize" "width" "height")
                      :test 'string-equal))
        (progn
          ;; Check whether image exists in imgdb.
          ;; Get image location in imgdb-store.
          ;; Resize the image, if needed.
          ;; Send the (resized?) image back.
          (format nil "still under construction"))
        (not-found-page))))

(defun get-img-id-from-uri (uri)
  (register-groups-bind (img-id)
      ("/img-urls/([0-9a-z]*)(\\?|$)" uri)
    img-id))

(push (create-regex-dispatcher "/img-urls/.*" 'img-url-handler)
      *dispatch-table*)
