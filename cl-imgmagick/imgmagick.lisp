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

(defgeneric read-image (filename &key channels read-pixels))
(defgeneric write-image (image &optional filename))

(defgeneric image-get-property (image property))
(defgeneric image-orientation (image))
(defgeneric image-original-date (image))
(defgeneric image-date (image))
(defgeneric image-height (image))
(defgeneric image-width (image))

(defclass image ()
  ((filename :reader image-filename
             :type string
             :initarg :filename
             :initform (error "Filename not provided."))
   (pixels :reader image-pixels
           :type foreign-pointer)
   (channels :reader image-channels
             :type string
             :initarg :channels
             :initform (error "Channels not provided."))
   (wand :type foreign-pointer
         :reader image-wand)))

(defmethod initialize-instance :after
    ((self image) &key wand pixels)
  (unless pixels (error "Pixels not provided."))
  (unless wand (error "Wand not provided."))
  (when (null-pointer-p wand) (error "Wand pointer is null."))

  (unless (null-pointer-p pixels)
    (finalize self (lambda ()
                     (foreign-free pixels))))
  (finalize self (lambda () 
                   (destroy-magick-wand wand)))

  (setf (slot-value self 'pixels) pixels)
  (setf (slot-value self 'wand) wand))

(defmethod read-image
    (filename &key (channels *default-channels*) (read-pixels nil))
  (check-magick-initialized)
  (let ((wand (new-magick-wand)))
    (magick-read-image wand (namestring filename))
    (let* ((width (magick-get-image-width wand))
           (height (magick-get-image-height wand))
           (pixels
            (if read-pixels
                (foreign-alloc :char :initial-element 0 :count 
                               (* height width (length channels)))
                (null-pointer))))
      (when read-pixels
        (magick-get-image-pixels wand 0 0 width height
                                 channels :char-pixel pixels))
      (make-instance 'image
                     :filename filename
                     :pixels pixels :channels channels
                     :wand wand))))

(defmethod write-image ((image image) &optional filename)
  (check-magick-initialized)
  (magick-write-image (image-wand image)
                      (when filename (namestring filename))))

(defparameter *exif-date-scanner*
  (create-scanner "(\\d+):(\\d+):(\\d+) (\\d+):(\\d+):(\\d+)"
                  :single-line-mode t))

(defun parse-exif-date (date-str)
  (register-groups-bind (year month day hour minute second)
      (*exif-date-scanner* date-str)
    (list year month day hour minute second)))

(defmethod image-get-property ((image image) property)
  (let ((result (magick-get-image-property (image-wand image) property)))
    (magick-relinquish-memory (second result))
    (first result)))

(defmethod image-orientation ((image image))
  (image-get-property image "Exif:Orientation"))
(defmethod image-original-date ((image image))
  (parse-exif-date (image-get-property image "Exif:DateTimeOriginal")))
(defmethod image-date ((image image))
  (parse-exif-date (image-get-property image "Exif:DateTime")))
(defmethod image-height ((image image))
  (magick-get-image-height (image-wand image)))
(defmethod image-width ((image image))
  (magick-get-image-width (image-wand image)))

(unless *magick-initialized* 
  (magick-initialize))
