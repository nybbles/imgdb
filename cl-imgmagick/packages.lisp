(in-package #:cl-user)

(defpackage :cl-imgmagick
  (:use :cl :cffi :cl-ppcre :trivial-garbage)
  (:export :with-magick-wand :with-image-blob
           :magick-ping-image :magick-read-image
           :magick-adaptive-resize-image :magick-crop-image
           :magick-thumbnail-image
           :magick-get-image-blob
           :image-date :image-original-date
           :image-width :image-height))
