(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun generate-date-cloud (constraints dbconn)
  (with-html-output-to-string (output nil)
    (:h3 :align "center" "Pictures by:")
    (:h4 :align "left" "year")
    (:p :align "left"
        :class "date-cloud-year"
        (str
         (generate-tag-cloud
          (get-tags-for-date-cloud "year" constraints :database dbconn))))
    (:h4 :align "left" "month")
    (:p :align "left"
        :class "date-cloud-month"
        (str
         (generate-tag-cloud
          (get-tags-for-date-cloud "month" constraints
                                   :database dbconn :order :asc))))
    (:h4 :align "left" "day")
    (:p :align "left"
        :class "date-cloud-day"
        (str
         (generate-tag-cloud
          (get-tags-for-date-cloud "day" constraints :database dbconn))))))

(defun tag-value-to-display-name (tag tag-value)
  (cond
    ((equal tag "year")
     (translate-year-to-year-str tag-value))
    ((equal tag "month")
     (translate-month-to-month-str tag-value))
    ((equal tag "day")
     (translate-day-to-day-str tag-value))))

(defun get-tags-for-date-cloud (tag constraints &key database (order :desc))
  (let ((pruned-constraints (remove-constraint constraints :name tag)))
    (get-tags tag constraints
              :tag-value-fn
              #'(lambda (x) (if (null (first x)) "undated" (first x)))
              :tag-quantity-fn #'second
              :get-tags-fn
              #'(lambda ()
                  (select-num-imgs-for-tag-type
                   tag pruned-constraints
                   :database database :order order)))))

(defun get-tags
    (tag constraints
     &key tag-value-fn tag-quantity-fn get-tags-fn)
  (let ((pruned-constraints (remove-constraint constraints :name tag)))
    (mapcar
     #'(lambda (x)
         (let* ((tag-value (funcall tag-value-fn x))
                (tag-quantity (funcall tag-quantity-fn x))
                (display-name (tag-value-to-display-name tag tag-value))
                (tag-is-active (find-constraint constraints
                                                :name tag :value tag-value))
                (tag-link
                 (get-query-link-for-constraints
                  (if tag-is-active
                      pruned-constraints
                      (cons (make-constraint tag tag-value)
                            pruned-constraints)))))
           (list display-name tag-link tag-quantity tag-is-active)))
     (funcall get-tags-fn))))

(defun select-num-imgs-for-tag-type
    (tag-type constraints &key database (order :desc))
  (let* ((tag-type-column (sql-expression
                           :attribute (intern (string-upcase tag-type))))
         (select-columns (list tag-type-column [count [*]]))
         (where-clause (translate-constraints-to-sql constraints))
         (order-by-clause (list (list tag-type-column order)))
         (result (select-img-records select-columns
                                     :where where-clause
                                     :group-by tag-type-column
                                     :order-by order-by-clause
                                     :database database))
         (last-tag (car (last result))))
    ;; TODO (NM): Find out what this code is doing and document it.
    (if (and (eq order :asc) (not (null result)) (null (car last-tag)))
        (push last-tag (subseq result 0 (- (length result) 1)))
        result)))

(restore-sql-reader-syntax-state)
