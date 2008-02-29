(in-package :cl-imgmagick)

(define-foreign-library libWand
  (:darwin "libWand.dylib")
  (t (:default "libWand")))
(use-foreign-library libWand)

;;; MagickWand and MagickWand Image ffi

(defctype magickbool :boolean)

(defcfun ("MagickWandGenesis" magick-wand-genesis) :void)
(defcfun ("MagickWandTerminus" magick-wand-terminus) :void)
(defcfun ("NewMagickWand" new-magick-wand) :pointer)
(defcfun ("NewMagickWandFromImage" new-magick-wand-from-image)
    :pointer (image :pointer))
(defcfun ("ClearMagickWand" clear-magick-wand) :void (wand :pointer))
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

(defcfun ("GetImageFromMagickWand" get-image-from-magick-wand)
    :pointer (wand :pointer))
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
(defcfun ("MagickGetImageCompressionQuality"
          magick-get-image-compression-quality)
    :ulong (wand :pointer))
(defcfun ("MagickIdentifyImage" magick-identify-image) :string (wand :pointer))
    

(defcfun ("MagickGetNumberImages" magick-get-number-images)
    :long (wand :pointer))
(defcfun ("MagickHasNextImage" magick-has-next-image)
    magickbool (wand :pointer))
(defcfun ("MagickHasPreviousImage" magick-has-previous-image)
    magickbool (wand :pointer))
(defcfun ("MagickNextImage" magick-next-image) magickbool (wand :pointer))
(defcfun ("MagickPreviousImage" magick-previous-image)
    magickbool (wand :pointer))

; (with-foreign-object (severity :int)
;    (values (mem-ref severity :int)
;            (magick-get-exception *wand* severity)))

(defcfun ("MagickGetException" magick-get-exception)
    :string (wand :pointer) (severity :pointer))
(defcfun ("MagickClearException" magick-clear-exception)
    magickbool (wand :pointer))

(defcenum :storage-type
  :undefined-pixel
  :char-pixel
  :double-pixel
  :float-pixel
  :integer-pixel
  :long-pixel
  :quantum-pixel
  :short-pixel)

; (let* ((wand *wand*)
;       (x 0) (y 0)
;       (width (magick-get-image-width wand))
;       (height (magick-get-image-height wand))
;       (channels "RGB")
;       (storage-type :char-pixel))
;  (with-foreign-object (pixels :char (* width height (length channels)))
;    (magick-get-image-pixels wand x y width height
;                             channels storage-type pixels)))

;; Note: A lot more memory can be allocated on the heap than on the
;; stack. Sometimes the memory required to hold one 10MP picture
;; cannot be allocated on the stack and must be allocated on the heap.
(defcfun ("MagickGetImagePixels" magick-get-image-pixels)
    magickbool
  (wand :pointer)
  (x :long) (y :long) (width :long) (height :long)
  (channels :string) (storage-type :storage-type)
  (pixels :pointer))

; (let* ((wand *wand*)
;       (x 0) (y 0)
;       (width (magick-get-image-width wand))
;       (height (magick-get-image-height wand))
;       (channels "RGB")
;       (storage-type :char-pixel)
;       (numpixels (* width height (length channels)))
;       (pixels (foreign-alloc :uint8 :count numpixels)))
;  (dotimes (i numpixels)
;    (setf (mem-aref pixels :uint8 i) (random 256)))
;  (magick-set-image-pixels wand x y width height
;                           channels storage-type pixels))

(defcfun ("MagickSetImagePixels" magick-set-image-pixels)
    magickbool
  (wand :pointer)
  (x :long) (y :long) (width :long) (height :long)
  (channels :string) (storage-type :storage-type)
  (pixels :pointer))

;;; PixelWand ffi

(defcfun ("NewPixelWand" new-pixel-wand) :pointer)
(defcfun ("ClonePixelWand" clone-pixel-wand) :pointer (wand :pointer))
(defcfun ("ClearPixelWand" clear-pixel-wand) :void (wand :pointer))
(defcfun ("DestroyPixelWand" destroy-pixel-wand) :pointer (wand :pointer))

(defcfun ("PixelGetRed" pixel-get-red) :double (wand :pointer))
(defcfun ("PixelGetGreen" pixel-get-green) :double (wand :pointer))
(defcfun ("PixelGetBlue" pixel-get-blue) :double (wand :pointer))

(defcfun ("PixelSetRed" pixel-set-red) :void (wand :pointer) (red :double))
(defcfun ("PixelSetGreen" pixel-set-green)
    :void (wand :pointer) (green :double))
(defcfun ("PixelSetBlue" pixel-set-blue) :void (wand :pointer) (blue :double))
(defcfun ("PixelSetColor" pixel-set-color)
    magickbool (wand :pointer) (color :string))
