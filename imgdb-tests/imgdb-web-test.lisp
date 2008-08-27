(in-package :imgdb-tests)

(def-suite imgdb-web-tests)

(defparameter *invalid-constraints*
  '(("blah" . 2008)
    ("year" 2007)))
(defparameter *valid-constraints*
  '(("year" . 2008)
    ("month" . 4)
    ("day" . 29)
    ("day" . "undated")
    ("current" . 10)))

;;; Constraints tests
(in-suite imgdb-web-tests)

(test constraint-predicates
  (mapcar #'(lambda (x) (is (not (imgdb-web::valid-constraint? x))))
          *invalid-constraints*)
  (mapcar #'(lambda (x) (is (imgdb-web::valid-constraint? x)))
          *valid-constraints*)
  (is (not (imgdb-web::temporal-constraint? (car (last *valid-constraints*)))))
  (is (imgdb-web::constraints-equal
       (first *valid-constraints*) '("year" . 2008)))
  (is (not (imgdb-web::constraints-equal (first *valid-constraints*) nil)))
  (is (not (imgdb-web::constraints-equal (first *valid-constraints*)
                                         (second *valid-constraints*)))))

(test constraint-accessors
  (is (equal (imgdb-web::constraint-name (first *valid-constraints*)) "year"))
  (is (equal
       (imgdb-web::constraint-name (car (last *valid-constraints*))) "current"))
  (is (= (imgdb-web::constraint-value (first *valid-constraints*)) 2008))
  (is (equal (imgdb-web::constraint-value '("day" . "undated")) "undated"))
  (let ((constraints
         '(("year" . 2008) ("month" . 3)
           ("day" . 9) ("day" . 24) ("current" . 24))))
    ;; remove-constraints
    (is (tree-equal (imgdb-web::remove-constraint constraints :value 24)
                    (subseq constraints 0 3)))
    (is (tree-equal (imgdb-web::remove-constraint constraints :name "day")
                    (append (subseq constraints 0 2) (last constraints))))
    (is (tree-equal (imgdb-web::remove-constraint constraints
                                                  :name "day" :value 24)
                    (append (subseq constraints 0 3) (last constraints))))

    ;; find-constraint
    (is (tree-equal (imgdb-web::find-constraint constraints :value 24)
                    (nth 3 constraints)))
    (is (tree-equal (imgdb-web::find-constraint constraints :name "day")
                    (nth 2 constraints)))
    (is (tree-equal (imgdb-web::find-constraint constraints
                                                :name "current" :value 24)
                    (nth 4 constraints)))

    ;; get-current-constraint-value
    (is (= (imgdb-web::get-current-constraint-value constraints) 24))
    (is (= (imgdb-web::get-current-constraint-value
            (imgdb-web::remove-constraint constraints :name "current"))
           1))

    ;; get-query-link-for-constraints
    (is (equal
         (imgdb-web::get-query-link-for-constraints constraints 10)
         "/img-query?current=10&year=2008&month=3&day=9&day=24"))
    (is (equal
         (imgdb-web::get-query-link-for-constraints constraints)
         "/img-query?year=2008&month=3&day=9&day=24"))
    (is (equal
         (imgdb-web::get-query-link-for-constraints constraints 1)
         "/img-query?year=2008&month=3&day=9&day=24"))
    (is (equal
         (imgdb-web::get-query-link-for-constraints
          (cons '("year" . "undated") constraints))
         "/img-query?year=undated&year=2008&month=3&day=9&day=24"))

    ;; translate-constraint-to-url-get-parameter
    (is (equal
         (imgdb-web::translate-constraint-to-url-get-parameter '("year" . 2008))
         "year=2008"))
    (is (equal
         (imgdb-web::translate-constraint-to-url-get-parameter
          '("month" . "undated"))
         "month=undated"))

    (is (equal
         (imgdb-web::translate-get-parameters-to-constraints
          '(("month" . "12") ("year" . "undated")))
         '(("month" . 12) ("year" . "undated"))))))

(in-suite nil)
