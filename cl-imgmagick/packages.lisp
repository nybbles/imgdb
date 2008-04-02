(in-package #:cl-user)

(defpackage :cl-imgmagick
  (:use :cl :cffi :cl-ppcre :trivial-garbage)
  (:export :with-magick-wand :magick-ping-image
           :image-date :image-original-date
           :image-width :image-height))
