(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun create-thumbnail-grid (img-ids)
  (let* ((num-img-ids (length img-ids))
         (row-length (find-grid-row-length num-img-ids)))
    (with-html-output-to-string (output nil)
       (:table
        (str
         (apply #'concatenate 'string
                 (loop for ids = img-ids then (nthcdr row-length ids)
                    until (null ids)
                    collect
                    (create-thumbnail-grid-row ids row-length))))))))

(defun find-grid-row-length (num-img-ids)
  (case num-img-ids
    (5 5)
    (10 5)
    (20 5)
    (40 8)
    (50 10)
    (t (ceiling (sqrt num-img-ids)))))

(defun create-thumbnail-grid-row
    (img-ids &optional (row-length (length img-ids)))
  (with-html-output-to-string (output nil)
    (:tr
     (loop for img-id in img-ids
        for i from 1 to row-length
        do
        (let ((thumbnail-url (format nil "img-urls/thumbnails/~A" img-id))
              (img-url (format nil "img-urls/thumbnails/~A" img-id)))
          (htm (:td
                (:a :class "thumbnail" :href img-url
                    (:img :src thumbnail-url)))))))))

(defun create-links-for-query-resultset
    (constraints current img-set-size dbconn)
  (let* ((range (get-range-for-constraints
                 (translate-constraints-to-sql constraints)
                 dbconn))
         (intervals-list
          (collect-query-resultset-link-intervals
           current range img-set-size 10)))
    (with-html-output-to-string (output nil)
      (:span :class "query-resultset-links"
       (loop for intervals in intervals-list
          do (progn
               (loop for i in intervals
                  do (htm (:a
                           :href (get-query-resultset-link constraints i)
                           (str i))
                          (str " ")))
               (htm (:br))))))))

(defun get-query-resultset-link (constraints new-current)
  (concatenate
   'string
   "/img-query?current=" (write-to-string new-current)
   (if (null constraints)
       ""
       (apply
        #'concatenate 'string
        (mapcar
         #'(lambda (x)
             (concatenate
              'string "&" (translate-constraint-to-url-get-parameter x)))
         constraints)))))

(defun translate-constraint-to-url-get-parameter (constraint)
  (let ((name (first constraint))
        (value (second constraint)))
    (concatenate
     'string
     (string-downcase (write-to-string name)) "="
     (cond ((or (integerp value) (stringp value)) (write-to-string value))
           (t (signal "invalid type for constraint value"))))))

(defun in-range? (number range)
  (and (>= number (first range)) (<= number (second range))))

(defun get-range-for-constraints (where-clause database)
  (list 1
        (caar (select-img-records ([count [digest]])
                                  :where where-clause
                                  :database database))))

(defun get-img-ids (current num-img-ids where-clause database)
  (let ((offset (- (* num-img-ids (floor current num-img-ids)) 1)))
    (select-img-records ([digest])
                        :where where-clause
                        :order-by '(([year] :desc)
                                    ([month] :desc)
                                    ([day] :desc)
                                    [digest])
                        :limit num-img-ids
                        :offset offset
                        :flatp t
                        :database database)))

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
             (sql-operation '= (sql-expression :attribute name) value))
         result))))))

(defun collect-query-resultset-link-intervals
    (current range img-set-size &optional (max-num-intervals img-set-size))
  (loop for i from 1
       for intervals = (collect-intervals current range i
                                          img-set-size max-num-intervals)
       collect intervals
       until (not (= (length intervals) max-num-intervals))))

(defun collect-intervals
    (current range power &optional (base 10) (max-num-intervals base))
  (assert (evenp base))
  (let* ((interval-size (* base (expt max-num-intervals (- power 1))))
         (endpoints (get-endpoints current range power base max-num-intervals))
         (aligned-lower-endpoint
          (align-number (first endpoints) interval-size :up))
         (aligned-upper-endpoint
          (align-number (second endpoints) interval-size :down))
         (result '()))
    (when (not (= (second endpoints) aligned-upper-endpoint))
      (push (second endpoints) result))
    (loop for i from aligned-upper-endpoint
                downto aligned-lower-endpoint
                by interval-size
         do (push i result))
    (when (not (= (first endpoints) aligned-lower-endpoint))
      (push (first endpoints) result))
    result))

(defun get-endpoints
    (current range power &optional (base 10) (max-num-intervals base))
  (assert (evenp base))
  (let* ((num-side-intervals (- (/ max-num-intervals 2) 1))
         (interval-size (expt base power))
         (initial-interval (get-initial-interval current interval-size))
         (rhs (min (second range)
                   (+ (second initial-interval)
                      (* num-side-intervals interval-size))))
         (lhs (max (first range)
                   (- (first initial-interval)
                      (* num-side-intervals interval-size))))
         (rhs-spans-range (= rhs (second range)))
         (lhs-spans-range (= lhs (first range))))
    (if (or (and rhs-spans-range lhs-spans-range)
            (and (not rhs-spans-range) (not lhs-spans-range)))
        (list lhs rhs)
        (let* ((num-intervals (get-num-intervals lhs rhs interval-size))
               (intervals-needed (- max-num-intervals num-intervals 1))
               (distance-to-endpoint (* intervals-needed interval-size)))
          (cond
            ((and rhs-spans-range (not lhs-spans-range))
             (list (max (first range)
                        (- lhs distance-to-endpoint))
                   rhs))
            ((and (not rhs-spans-range) lhs-spans-range)
             (list lhs
                   (min (second range)
                        (+ rhs distance-to-endpoint))))
            (t (error :logic-error)))))))

(defun get-num-intervals (start end interval-size)
  (assert (< start end))
  (let ((aligned-rhs (align-number end interval-size :down))
        (aligned-lhs (align-number start interval-size :up)))
    (+ (/ (- aligned-rhs aligned-lhs) interval-size)
       (if (not (= aligned-rhs end)) 1 0)
       (if (not (= aligned-lhs start)) 1 0))))

(defun align-number (number interval-size direction)
  (* (ccase direction
       (:down (floor number interval-size))
       (:up (ceiling number interval-size)))
    interval-size))

(defun get-initial-interval (current interval-size)
  (if (= (mod current interval-size) 0)
      (list current (+ current interval-size))
      (mapcar #'(lambda (x)
                  (align-number current interval-size x))
              '(:down :up))))

(restore-sql-reader-syntax-state)
