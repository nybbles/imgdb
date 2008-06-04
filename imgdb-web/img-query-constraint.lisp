(in-package :imgdb-web)

(defun translate-constraints-to-sql (constraints &optional result)
  (cond
    ((and (null constraints) (null result)) nil)
    ((null constraints)
     (if (> (length result) 1)
         (apply #'sql-operation 'and result)
         (car result)))
    (t 
     (let* ((constraint (first constraints))
            (name (first constraint))
            (value (second constraint)))
       (translate-constraints-to-sql
        (rest constraints)
        (cons
         (if (and (equal name "year") (equal value "undated"))
             (sql-operation 'is (sql-expression :attribute name) 'null)
             (if (not (integerp value))
                 (error 'invalid-img-query-error)
                 (sql-operation '= (sql-expression :attribute name) value)))
         result))))))

(defun translate-get-parameters-to-constraints (params)
  (if (position-if-not #'is-valid-constraint params)
      (error 'invalid-img-query-error)
      (mapcar #'(lambda (x)
                  (if (is-valid-constraint x)
                      (list (car x)
                            (let ((value (cdr x)))
                              (handler-case (parse-integer value)
                                (parse-error () value))))
              params)))))

(defparameter *valid-constraints* '("year" "month" "day"))
(defun is-valid-constraint (x)
  (and (consp x) (stringp (car x)) (atom (cdr x))
       (member (car x) *valid-constraints* :test #'equal)))

(defun get-current-constraint-value (params)
  (let ((current-assoc (assoc "current" params :test #'equal)))
    (if current-assoc
        (handler-case (parse-integer (cdr current-assoc))
          (parse-error () (error 'invalid-img-query-error)))
        1)))
