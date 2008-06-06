(in-package :imgdb-web)

(defparameter *non-db-constraints* '("current"))
(defparameter *db-constraints* '("year" "month" "day"))
(defparameter *temporal-constraints* '("year" "month" "day"))
(defparameter *valid-constraints* (union *non-db-constraints* *db-constraints*))

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
            (let ((constraint (car constraints)))
              (if (not (valid-constraint? constraint))
                  (error 'invalid-img-query-error)
                  (translate-constraints-to-sql-helper
                   (cdr constraints)
                   (if (db-constraint? constraint)
                       (cons (translate-constraint-to-sql constraint)
                             result)
                       result))))))))
    (translate-constraints-to-sql-helper constraints nil)))

(defun translate-constraint-to-sql (constraint)
  (let ((name (constraint-name constraint))
        (value (constraint-value constraint))
        (temporal? (temporal-constraint? constraint)))
    (cond
      ((and temporal? (equal value "undated"))
       (sql-operation 'is
                      (sql-expression :attribute name) 'null))
      ((and temporal? (not (integerp value)))
       (error 'invalid-img-query-error))      
      (t (sql-operation '=
                        (sql-expression :attribute name)
                        value)))))

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
     (cond ((integerp value) (write-to-string value))
           ((stringp value) value)
           (t (error 'invalid-img-query-error))))))

(defun get-query-link-for-constraints (constraints &optional (new-current 1))
  (setf constraints (remove-constraint constraints :name "current"))
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

(defun well-formed-constraint? (x)
  (and (consp x) (stringp (car x)) (atom (cdr x))))

(defun valid-constraint? (x)
  (if (and (well-formed-constraint? x)
           (member (car x) *valid-constraints* :test #'equal))
      t nil))

(defun db-constraint? (x)
  (if (and (well-formed-constraint? x)
           (member (car x) *db-constraints* :test #'equal))
      t nil))

(defun temporal-constraint? (x)
  (if (and (well-formed-constraint? x)
           (member (car x) *temporal-constraints* :test #'equal))
      t nil))

(defun get-current-constraint-value (constraints)
  (let ((current-constraint (find-constraint constraints :name  "current")))
    (if current-constraint
        (let ((value (cdr current-constraint)))
          (unless (integerp value)
            (error 'invalid-img-query-error))
          value)
        1)))

(defun make-constraint (name value)
  (when (not (stringp name))
    (error "Invalid name for constraint"))
  (cons name
        (cond
          ((integerp value) value)
          ((stringp value)
           (handler-case (parse-integer value)
             (parse-error () value)))
          (t (error "Invalid value for constraint")))))

(defun find-constraint (constraints &key name value)
  (find-if #'(lambda (x)
               (and (if (null name) t (equal (constraint-name x) name))
                      (if (null value) t (equal (constraint-value x) value))))
           constraints))

(defun remove-constraint (constraints &key name value)
  (remove-if #'(lambda (x)
                 (and (if (null name) t (equal (constraint-name x) name))
                      (if (null value) t (equal (constraint-value x) value))))
             constraints))

(defun constraints-equal (constraint1 constraint2)
  (if (and (equal (constraint-name constraint1) (constraint-name constraint2))
           (equal (constraint-value constraint2)
                  (constraint-value constraint2)))
      t nil))

(defun constraint-name (constraint)
  (car constraint))
(defun constraint-value (constraint)
  (cdr constraint))
