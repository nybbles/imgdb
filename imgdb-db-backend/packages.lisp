(in-package #:cl-user)

(defpackage :imgdb-db-backend
  (:use :cl :cl-fad :ironclad :clsql)
  (:export :index-directory))
