(in-package :imgdb-web)

(defun img-query-page ()
  (let* ((params (get-parameters))
         (constraints (translate-get-parameters-to-constraints params))
         (current
          (handler-case (get-current-value params)
            (parse-error () 1))))
    (with-database (dbconn *imgdb-store-db-conn-spec*
                           :database-type *imgdb-store-db-type*
                           :pool t)
      (with-html-output-to-string (output nil :prologue t)
        (:html
         (str (generate-html-head "imgdb"))
         (:body
          (:div :id "signin"
                (:a :href "login.htm" "Sign in"))
          (:br)
          (:br)
          (:div :id "front-content"
                (:h1 :align "center" "imgdb")
                (:p
                 "It's still under construction, so everything looks terrible.")
                (str
                 (create-thumbnail-grid
                  (get-img-ids current 40
                               (translate-constraints-to-sql constraints)
                               dbconn)))
                (:br)
                (str
                 (create-links-for-query-resultset
                  constraints current 40 dbconn)))
          (:div :id "date-cloud"
                (:h3 :align "center" "Pictures by year")
                (str (generate-tag-cloud
                      (convert-num-imgs-by-year-results-to-tag-list
                       (select-num-imgs-by-year dbconn)))))))))))

(defun get-current-value (params)
  (let ((current-assoc (assoc "current" params :test #'equal)))
    (if current-assoc
        (parse-integer (cdr current-assoc))
        1)))

(defparameter *valid-constraints* '("year" "month" "day"))
(defun is-valid-constraint (x)
  (and (consp x) (stringp (car x)) (atom (cdr x))
       (member (car x) *valid-constraints* :test #'equal)))

(defun translate-get-parameters-to-constraints (params)
  (mapcar #'(lambda (x)
                (list (intern (string-upcase (car x)))
                      (let ((value (cdr x)))
                        (handler-case (parse-integer value)
                          (parse-error () value)))))
          (remove-if-not #'is-valid-constraint params)))
