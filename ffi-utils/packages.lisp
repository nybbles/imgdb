(in-package #:cl-user)

(defpackage :ffi-utils
  (:use :cl :cffi)
  (:export :foreign-buffer-to-vector
           :iterate-over-foreign-buffer))
