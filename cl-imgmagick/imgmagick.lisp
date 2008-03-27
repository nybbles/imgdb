(in-package :cl-imgmagick)

(defparameter *exif-scanner*
  (create-scanner "Exif:([^:]*):(.*)" :single-line-mode t))
(defvar *magick-initialized* nil)
(defparameter *default-channels* "RGB")

(defun magick-initialize ()
  (when *magick-initialized*
    (error "cl-imgmagick has already been initialized."))
  (setf *magick-initialized* t)
  (magick-wand-genesis))

(defun magick-terminate ()
  (unless *magick-initialized*
    (error "cl-imgmagick has not been initialized."))
  (setf *magick-initialized* nil)
  (magick-wand-terminus))

(defun check-magick-initialized ()
  (unless  *magick-initialized*
    (error "cl-imgmagick has not been initialized.")))

(defun remove-whitespace (string)
  (regex-replace "\\s+$" (regex-replace "^\\s+" string "") ""))

(defmacro with-magick-wand (wand &body body)
  `(let ((,wand (new-magick-wand)))
     ,@body
     (destroy-magick-wand ,wand)))

(defmacro with-image-pixels
    ((wand pixels &optional (channels *default-channels*)) &body body)
  (let ((image-width-name (gensym "image-width"))
        (image-height-name (gensym "image-height")))
  `(let ((,image-width-name (magick-get-image-width ,wand))
         (,image-height-name (magick-get-image-height ,wand)))
     (with-foreign-object
         (,pixels :char (* ,image-width-name ,image-height-name))
       (magick-get-image-pixels ,wand 0 0 ,image-width-name ,image-height-name
                                ,channels :char-pixel ,pixels)
       ,@body))))

(defparameter *exif-date-scanner*
  (create-scanner "(\\d+):(\\d+):(\\d+) (\\d+):(\\d+):(\\d+)"
                  :single-line-mode t))

(defun parse-exif-date (date-str)
  (register-groups-bind (year month day hour minute second)
      (*exif-date-scanner* date-str)
    (list year month day hour minute second)))

(defun image-get-property (wand property)
  (let ((result (magick-get-image-property wand property)))
    (magick-relinquish-memory (second result))
    (first result)))

(defun image-orientation (wand)
  (image-get-property wand "Exif:Orientation"))
(defun image-original-date (wand)
  (parse-exif-date (image-get-property wand "Exif:DateTimeOriginal")))
(defun image-date (wand)
  (parse-exif-date (image-get-property wand "Exif:DateTime")))

(unless *magick-initialized* 
  (magick-initialize))
