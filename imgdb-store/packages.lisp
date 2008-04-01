(in-package #:cl-user)

(defpackage :imgdb-store
  (:use :cl :cl-fad :ironclad :clsql :cl-imgmagick :cl-ppcre)
  (:export :connect-to-dbserver :img-table-exists :create-img-table
           :index-img-drop :select-img-record
           :enable-sql-reader-syntax :disable-sql-reader-syntax))
