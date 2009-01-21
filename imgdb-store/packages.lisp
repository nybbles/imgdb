(in-package #:cl-user)

(defpackage :imgdb-store
  (:use :cl :cl-fad :ironclad :clsql :cl-ppcre :hunchentoot-mp
        :cl-imgmagick :ffi-utils)
  (:export :create-imgdb-dbconn :destroy-imgdb-dbconn
           :img-table-exists :create-img-table :drop-img-table
           :create-all-tables
           :index-img-drop :select-img-records :count-img-records
           :img-record-exists :update-img-records
           :enable-sql-reader-syntax :disable-sql-reader-syntax
           :locally-enable-sql-reader-syntax
           :locally-disable-sql-reader-syntax
           :restore-sql-reader-syntax-state
           :sql-operation :sql-expression
           :with-database :with-transaction
           :with-thumbnail :with-resized-image
           :get-img-tags :add-img-tags :delete-img-tags
           :*img-resize-cache-conn-spec* :*img-resize-cache-conn-type*))
