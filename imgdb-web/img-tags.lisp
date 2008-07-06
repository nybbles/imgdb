(in-package :imgdb-web)

(defun add-img-tags-handler ()
  (let ((json (raw-post-data :force-text t)))
    (setf (content-type *reply*) "application/json")
    (setf (log-file) "/Users/nimalan/Desktop/imgdb-log")
    (log-message* "add-img-tags-handler received post data: [~A]~%"
                  json)
    "/* {\"tags\" : [\"blah\", \"zah\"]} */"))

(defun delete-img-tags-handler ())

(defparameter *json-object-regex* "^\\s*{\\s*(.*)\\s*}\\s*$")
(defparameter *json-array-regex* "^\\s*[\\s*(.*)\\s*]\\s*$")
(defparameter *json-string-regex* "^\\s*\"(.*)\"\\s*$")
(defparameter *json-number-regex*
  "^\\s*(-?(0\\.[0-9]+|[1-9][0-9]*(\\.[0-9]+)?)([eE][+-]?[0-9]+)?)\\s*$")

(defun from-json (json-str &optional (result nil)))

(defun number-from-json (json-str)
  (register-groups-bind (json-num)
      (*json-number-regex* json-str)
    (assert (not (null json-num)))
    (let ((lisp-num
           (unless (null json-num) (read-from-string json-num))))
      (if (numberp lisp-num) lisp-num nil))))
