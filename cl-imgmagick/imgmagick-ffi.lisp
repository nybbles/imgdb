(in-package :cl-imgmagick)

(define-foreign-library libWand
  (:darwin "libWand.dylib")
  (t (:default "libWand")))
(use-foreign-library libWand)

(defctype magickbool :boolean)

(defcfun ("MagickWandGenesis" magick-wand-genesis) :void)
(defcfun ("MagickWandTerminus" magick-wand-terminus) :void)
(defcfun ("NewMagickWand" new-magick-wand) :pointer)
(defcfun ("NewMagickWandFromImage" new-magick-wand-from-image)
    :pointer (image :pointer))
(defcfun ("DestroyMagickWand" destroy-magick-wand) :pointer (wand :pointer))
(defcfun ("IsMagickWand" is-magick-wand) magickbool (wand :pointer))

(defcfun ("MagickReadImage" magick-read-image)
    magickbool (wand :pointer) (filename :string))
(defcfun ("MagickWriteImage" magick-write-image)
    magickbool (wand :pointer) (filename :string))
(defcfun ("MagickPingImage" magick-ping-image)
    magickbool (wand :pointer) (filename :string))
(defcfun ("MagickNewImage" magick-new-image)
    magickbool
  (wand :pointer) (width :long) (height :long) (background :pointer))
(defcfun ("MagickDestroyImage" magick-destroy-image) :pointer (image :pointer))

(defcfun ("MagickDisplayImage" magick-display-image)
    magickbool (wand :pointer) (server :string))
(defcfun ("MagickDisplayImages" magick-display-images)
    magickbool (wand :pointer) (server :string))

(defcfun ("MagickGetImageFilename" magick-get-image-filename)
    :string (wand :pointer))
(defcfun ("MagickGetImageFormat" magick-get-image-format)
    :string (wand :pointer))
(defcfun ("MagickGetImageHeight" magick-get-image-height) :long (wand :pointer))
(defcfun ("MagickGetImageWidth" magick-get-image-width) :long (wand :pointer))

(defcfun ("MagickGetNumberImages" magick-get-number-images)
    :long (wand :pointer))
(defcfun ("MagickHasNextImage" magick-has-next-image)
    magickbool (wand :pointer))
(defcfun ("MagickHasPreviousImage" magick-has-previous-image)
    magickbool (wand :pointer))
(defcfun ("MagickNextImage" magick-next-image) magickbool (wand :pointer))
(defcfun ("MagickPreviousImage" magick-previous-image)
    magickbool (wand :pointer))

;; (with-foreign-object (severity :int)
;;    (values (mem-ref severity :int)
;;            (magick-get-exception *wand* severity)))

(defcfun ("MagickGetException" magick-get-exception)
    :string (wand :pointer) (severity :pointer))
(defcfun ("MagickClearException" magick-clear-exception)
    magickbool (wand :pointer))
