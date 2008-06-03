(in-package :imgdb-web)

(define-condition invalid-img-query-error () ())

(defun img-query-page ()
  (handler-case
      (let* ((params (get-parameters))
             (constraints (translate-get-parameters-to-constraints params))
             (current (get-current-value params)))
        (with-database (dbconn *imgdb-store-db-conn-spec*
                               :database-type *imgdb-store-db-type*
                               :pool t :if-exists :old)
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
                           (select-num-imgs-by-year dbconn))))))))))
    (invalid-img-query-error () (not-found-page))))

(defun get-current-value (params)
  (let ((current-assoc (assoc "current" params :test #'equal)))
    (if current-assoc
        (handler-case (parse-integer (cdr current-assoc))
          (parse-error () (error 'invalid-img-query-error)))
        1)))

(defparameter *valid-constraints* '("year" "month" "day"))
(defun is-valid-constraint (x)
  (and (consp x) (stringp (car x)) (atom (cdr x))
       (member (car x) *valid-constraints* :test #'equal)))

(defun translate-get-parameters-to-constraints (params)
  (if (position-if-not #'is-valid-constraint params)
      (error 'invalid-img-query-error)
      (mapcar #'(lambda (x)
                  (list (car x)
                        (let ((value (cdr x)))
                          (handler-case (parse-integer value)
                            (parse-error () value)))))
              params)))
