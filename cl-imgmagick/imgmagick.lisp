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

(defun magick-get-exif (wand)
  (let ((exif-data (magick-identify-image wand))
        (exif-table (make-hash-table :test 'equal)))
    (dolist (line (split #\Newline exif-data))
      (register-groups-bind (field value) (*exif-scanner* line)
        (setf (gethash (remove-whitespace field) exif-table)
              (remove-whitespace value))))
    exif-table))

(defun remove-whitespace (string)
  (regex-replace "\\s+$" (regex-replace "^\\s+" string "") ""))

(defgeneric read-image (filename &key channels read-pixels))
(defgeneric write-image (image &optional filename))

(defgeneric orientation (image))
(defgeneric date (image))
(defgeneric height (image))
(defgeneric width (image))

(defclass image ()
  ((exif :reader exif
         :type hash-table
         :initarg :exif
         :initform (error "EXIF not provided."))
   (filename :reader filename
             :type string
             :initarg :filename
             :initform (error "Filename not provided."))
   (pixels :reader pixels
           :type foreign-pointer)
   (channels :reader channels
             :type string
             :initarg :channels
             :initform (error "Channels not provided."))
   (wand :type foreign-pointer
         :reader wand)))

(defmethod initialize-instance :after
    ((self image) &key wand pixels)
  (unless pixels (error "Pixels not provided."))
  (unless wand (error "Wand not provided."))
  (when (null-pointer-p wand) (error "Wand pointer is null."))

  (unless (null-pointer-p pixels)
    (finalize self (lambda ()
                     (print "freeing pixels")
                     (foreign-free pixels))))
  (finalize self (lambda () 
                   (print "freeing wand")
                   (destroy-magick-wand wand)))

  (setf (slot-value self 'pixels) pixels)
  (setf (slot-value self 'wand) wand))

(defmethod read-image
    (filename &key (channels *default-channels*) (read-pixels nil))
  (check-magick-initialized)
  (let ((wand (new-magick-wand)))
    (magick-read-image wand filename)
    (let* ((exif (magick-get-exif wand))
           (width (magick-get-image-width wand))
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
                     :exif exif :filename filename
                     :pixels pixels :channels channels
                     :wand wand))))

(defmethod write-image ((image image) &optional filename)
  (check-magick-initialized)
  (magick-write-image (wand image)
                      (when filename filename)))

(defparameter *exif-date-scanner*
  (create-scanner "(\\d+):(\\d+):(\\d+) (\\d+):(\\d+):(\\d+)"
                  :single-line-mode t))

(defun parse-exif-date (date-str)
  (register-groups-bind (year month day hour minute second)
      (*exif-date-scanner* date-str)
    (list year month day hour minute second)))

(defmethod orientation ((image image))
  (gethash "Orientation" (exif image)))
(defmethod date ((image image))
  (parse-exif-date (gethash "DateTimeDigitized" (exif image))))
(defmethod height ((image image))
  (gethash "ExifImageLength" (exif image)))
(defmethod width ((image image))
  (gethash "ExifImageWidth" (exif image)))
