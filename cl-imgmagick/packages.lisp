(in-package #:cl-user)

(defpackage :cl-imgmagick
  (:use :cl :cffi :cl-ppcre :trivial-garbage)
  (:export read-image image-date))
