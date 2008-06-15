(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun img-view-page-js-init (img-date-picker-id img-date)
  (format
   nil "dijit.byId('~A').setValue(~A)"
   img-date-picker-id
   (if (null img-date)
       "''"
       (let ((year (first img-date))
             (month (second img-date))
             (day (third img-date)))
         (format nil "GetLocalizedDateString(~D, ~D, ~D)"
                 year (- month 1) day)))))

(defun img-view-page ()
  (let ((img-id (cdr (assoc "id" (get-parameters) :test #'equal))))
    (with-database (dbconn *imgdb-store-db-conn-spec*
                           :database-type *imgdb-store-db-type*
                           :pool t :if-exists :old)
      (let ((date (select-img-date img-id dbconn))
            (title (format nil "imgdb - ~A" img-id)))
        (with-html-output-to-string (output nil :prologue t)
          (:html
           (str
            (generate-html-head
             title
             :js-impl :dojo
             :js-extras
             '((:dojo-require
                "dojo.parser" "dijit.InlineEditBox" "dijit.form.DateTextBox"
                "dijit.form.TextBox" "dijit.form.Textarea")
               (:js-include
                "/js/utils.js" "/js/imgdb-web.js")
               (:dojo-require "imgdb-web.widget.TagEntryDisplayBox"))))
           (:body
            :class "tundra"
            :onload (img-view-page-js-init "image-date" date)
            (:h1 :align "center" "imgdb")
            (:div
             :id "img-view-image"
             (:a :href (format nil "/img-urls/~A" img-id)
                 (:img :src (format nil "/img-urls/~A?width=500" img-id))))
            (:div
             :id "img-view-image-info"
             (:h3
              :autoSave "false"
              :dojoType "dijit.InlineEditBox"
              "Untitled")
             (:p
              :autoSave "false"
              :width "200px"
              :dojoType "dijit.InlineEditBox"
              :editor "dijit.form.Textarea"
              "Enter a description.")
             (:br)
             "Taken on: "
             (:span
              :id "image-date"
              :autoSave "false"
              :width "200px"
              :dojoType "dijit.InlineEditBox"
              :editor "dijit.form.DateTextBox"
              :noValueIndicator "undated"
              :editorParams
              "{constraints: {formatLength:'long'}}")
             (:br)
             (:br)
             (:br)
             (:h4 "Tags:")
             (:span
              :dojoType "imgdb-web.widget.TagEntryDisplayBox")))))))))
            
(defun select-img-date (img-id dbconn)
  (let ((result
         (car (select-img-records (list [year] [month] [day])
                                  :where [= [digest] img-id]
                                  :flatp t
                                  :database dbconn))))
    (if (position-if-not #'null result)
        result
        nil)))

(restore-sql-reader-syntax-state)
