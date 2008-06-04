(in-package :imgdb-web)

(defun translate-constraints-to-sql (constraints)
  (labels
      ((translate-constraints-to-sql-helper (constraints result)
         (cond
           ((and (null constraints) (null result)) nil)
           ((null constraints)
            (if (> (length result) 1)
                (apply #'sql-operation 'and result)
                (car result)))
           (t
            (let ((constraint (first constraints)))
              (cond
                ((not (valid-constraint? constraint))
                 (error 'invalid-img-query-error))
                ((not (db-constraint? constraint))
                 (translate-constraints-to-sql-helper (cdr constraints) result))
                (t
                 (let ((name (car constraint))
                       (value (cdr constraint)))
                   (translate-constraints-to-sql-helper
                    (cdr constraints)
                    (cons
                     (if (and (equal name "year") (equal value "undated"))
                         (sql-operation 'is
                                        (sql-expression :attribute name) 'null)
                         (if (not (integerp value))
                             (error 'invalid-img-query-error)
                             (sql-operation '=
                                            (sql-expression :attribute name)
                                            value)))
                     result))))))))))
    (translate-constraints-to-sql-helper constraints nil)))

(defun translate-get-parameters-to-constraints (params)
  (mapcar #'(lambda (x)
              (let ((constraint
                     (cons (car x)
                           (let ((value (cdr x)))
                             (unless (stringp value)
                               (error 'invalid-img-query-error))
                             (handler-case (parse-integer value)
                               (parse-error () value))))))
                (unless (valid-constraint? constraint)
                  (error 'invalid-img-query-error))
                constraint))
          params))

(defun translate-constraint-to-url-get-parameter (constraint)
  (let ((name (car constraint))
        (value (cdr constraint)))
    (concatenate
     'string name "="
     (cond ((or (integerp value) (stringp value)) (write-to-string value))
           (t (error 'invalid-img-query-error))))))

(defun get-query-link-for-constraints (constraints &optional (new-current 1))
  (let ((constraints
         (if (= new-current 1)
             constraints
             (cons (cons "current" new-current) constraints))))
    (if (null constraints)
        "/"
        (loop
           for constraint in constraints
           for result =
             (concatenate 'string "/img-query?"
                          (translate-constraint-to-url-get-parameter
                           constraint))
           then
           (concatenate
            'string result "&"
            (translate-constraint-to-url-get-parameter constraint))
           finally (return result)))))

(defparameter *non-db-constraints* '("current"))
(defparameter *db-constraints* '("year" "month" "day"))
(defparameter *valid-constraints* (union *non-db-constraints* *db-constraints*))

(defun valid-constraint? (x)
  (and (well-formed-constraint? x)
       (member (car x) *valid-constraints* :test #'equal)))

(defun db-constraint? (x)
  (and (well-formed-constraint? x)
       (member (car x) *db-constraints* :test #'equal)))

(defun well-formed-constraint? (x)
  (and (consp x) (stringp (car x)) (atom (cdr x))))

(defun get-current-constraint-value (params)
  (let ((current-assoc (assoc "current" params :test #'equal)))
    (if current-assoc
        (let ((value (cdr current-assoc)))
          (unless (integerp value)
            (error 'invalid-img-query-error))
          value)
        1)))

(defun make-constraint (name value)
  (assert (stringp name))
  (assert (or (integerp value) (stringp value)))
  (cons name
        (handler-case (parse-integer value)
          (parse-error () value))))
