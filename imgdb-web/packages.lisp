(in-package #:cl-user)

(defpackage :imgdb-web
  (:use :cl :hunchentoot :cl-who :cl-ppcre
        :cl-imgmagick :imgdb-store :ffi-utils)
  (:export :nil))
