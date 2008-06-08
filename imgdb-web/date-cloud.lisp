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
          (get-tags "year" constraints :database dbconn))))
    (:h4 :align "left" "month")
    (:p :align "left"
        :class "date-cloud-month"
        (str
         (generate-tag-cloud
          (get-tags "month" constraints :database dbconn :order :asc))))
    (:h4 :align "left" "day")
    (:p :align "left"
        :class "date-cloud-day"
        (str
         (generate-tag-cloud
          (get-tags "day" constraints :database dbconn))))))

(defun get-friendly-tag-value (tag tag-value)
  (cond
    ((equal tag "year")
     (translate-year-to-year-str tag-value))
    ((equal tag "month")
     (translate-month-to-month-str tag-value))
    ((equal tag "day")
     (translate-day-to-day-str tag-value))))

(defun get-tags (tag constraints &key database (order :desc))
  (let ((pruned-constraints (remove-constraint constraints :name tag)))
    (mapcar
     #'(lambda (x)
         (let* ((tag-value (if (null (first x)) "undated" (first x)))
                (friendly-tag-value (get-friendly-tag-value tag (first x)))
                (tag-quantity (second x))
                (tag-is-active (find-constraint constraints
                                                :name tag :value tag-value))
                (tag-link
                 (get-query-link-for-constraints
                  (if tag-is-active
                      pruned-constraints
                      (cons (make-constraint tag tag-value)
                            pruned-constraints)))))
           (list friendly-tag-value tag-link tag-quantity tag-is-active)))
     (select-num-imgs-for-tag-type tag
                                   pruned-constraints
                                   :database database :order order))))

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
    (if (and (eq order :asc) (null (car last-tag)))
        (push last-tag (subseq result 0 (- (length result) 1)))
        result)))

(restore-sql-reader-syntax-state)
