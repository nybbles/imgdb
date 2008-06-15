(in-package :imgdb-web)

(setf *attribute-quote-char* #\")

(defun generate-html-head (title &key (js-impl nil) (js-extras nil))
  (with-html-output-to-string (output nil)
    (:head
     (:link :rel "stylesheet" :type "text/css"
            :href "css/imgdb-web.css")
     (:title (str title))
     (str (generate-js-include js-impl js-extras)))))

(defun generate-js-include (js-impl js-extras)
  (if js-impl
      (ccase js-impl
        (:dojo (generate-js-include-dojo js-extras)))
      ""))

(defun generate-js-include-dojo (js-extras)
  (push '(:css-import
          "/js/dojo/dojo-1.1.1/dijit/themes/tundra/tundra.css")
        js-extras)
  (with-html-output-to-string (output nil)
    (:script :type "text/javascript"
             :src "/js/dojo/dojo-1.1.1/dojo/dojo.js"
             :djConfig "parseOnLoad: true")
    (loop for thing in js-extras
       do
       (str
        (cond
          ((eq (car thing) :dojo-require)
           (require-dojo-modules (cdr thing)))
          ((eq (car thing) :js-include)
           (include-js-files (cdr thing)))
          ((eq (car thing) :css-import)
           (import-css-files (cdr thing))))))))

(defun require-dojo-modules (dojo-modules)
  (with-html-output-to-string (output nil :prologue nil)
    (:script :type "text/javascript"
             (loop for module in dojo-modules
                do
                (str (concatenate 'string "dojo.require(\"" module "\");"))))))

(defun include-js-files (js-files)
  (with-html-output-to-string (output nil :prologue nil)
    (loop for file in js-files
         do
         (htm (:script :type "text/javascript"
                       :src file)))))

(defun import-css-files (css-files)
  (with-html-output-to-string (output nil :prologue nil)
    (:style :type "text/css"
             (loop for file in css-files
                do
                (str (concatenate 'string "@import '" file "';"))))))

(defun translate-date-to-date-str (date &key (type :short))
  (assert (or (eq type :short) (eq type :long)))
  (let ((year (first date))
        (month (second date))
        (day (third date)))
    (cond ((and (null month) (not (null year)) (not (null day)))
           (translate-year-to-year-str year))
          ((and (null day) (null month) (null year)) "undated")
          (t
           (format
            nil "~{~#[~;~A~:;~A ~]~}"
            (remove-if #'null
                       (list
                        (if (not (null day))
                            (translate-day-to-day-str day :type type) nil)
                        (if (not (null month))
                            (translate-month-to-month-str month :type type) nil)
                        (if (not (null year))
                            (translate-year-to-year-str year) nil))))))))

(defun translate-year-to-year-str (year)
  (cond
    ((null year) "undated")
    ((integerp year) (write-to-string year))
    (t (error "Incorrect type for year: ~A" (type-of year)))))

(defun translate-month-to-month-str (month &key (type :short))
  (assert (or (eq type :short) (eq type :long)))
  (cond
    ((null month) "undated")
    ((integerp month)
     (if (eq type :long)
         (ccase month
           (1 "January")
           (2 "Febuary")
           (3 "March")
           (4 "April")
           (5 "May")
           (6 "June")
           (7 "July")
           (8 "August")
           (9 "September")
           (10 "October")
           (11 "November")
           (12 "December")
           (nil "undated"))
         (ccase month
           (1 "Jan")
           (2 "Feb")
           (3 "Mar")
           (4 "Apr")
           (5 "May")
           (6 "Jun")
           (7 "Jul")
           (8 "Aug")
           (9 "Sep")
           (10 "Oct")
           (11 "Nov")
           (12 "Dec")
           (nil "undated"))))
    (t (error "Incorrect type for month: ~A" (type-of month)))))

(defun translate-day-to-day-str (day &key (type :short))
  (assert (or (eq type :short) (eq type :long)))
  (cond
    ((null day) "undated")
    ((integerp day)
     (if (eq type :long)
         (format nil "~D<sup>~A</sup>"
                 day
                 (case (rem day 10)
                   (1 "st")
                   (2 "nd")
                   (3 "rd")
                   (t "th")))
         (write-to-string day)))
    (t (error "Incorrect type for day: ~A" (type-of day)))))
