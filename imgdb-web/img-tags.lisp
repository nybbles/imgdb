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
(defparameter *json-control-char-regex*
  "(\\\\([\\\/\\\\\\\"bfnrt]|u[0-9a-fA-F]{4}))")
(defparameter *json-string-regex*
  (concatenate 'string
               "^\\s*\\\"(([^\\\"\\\\]|"
               *json-control-char-regex*
               ")*)\\\"\\s*$"))
(defparameter *json-number-regex*
  "^\\s*(-?(0\\.[0-9]+|[1-9][0-9]*(\\.[0-9]+)?)([eE][+-]?[0-9]+)?)\\s*$")

(defun from-json (json-str &optional (result nil)))

(defun string-from-json (json-str &key (convert-control-chars nil))
  (register-groups-bind (json-str)
      (*json-string-regex* json-str)
    (if convert-control-chars
        (convert-json-control-chars-in-string-to-lisp json-str)
        json-str)))

(defun convert-json-control-chars-in-string-to-lisp (str)
  (let ((result (make-array 0 :element-type 'character :fill-pointer t)))
    (loop with start = 0
         with length = (length str)
         do (multiple-value-bind (match-start match-end)
                (scan *json-control-char-regex* str :start start)
              (if (null match-start)
                  (progn
                    (loop for i from start upto (- length 1)
                       do (vector-push-extend (aref str i) result))
                    (return result))
                  (progn
                    (loop for i from start upto (- match-start 1)
                         do (vector-push-extend (aref str i) result))
                    (loop for i across
                         (json-control-char-to-lisp
                          (subseq str match-start match-end))
                         do (vector-push-extend i result))
                    (setf start match-end)))))))

(defun json-control-char-to-lisp (json-control-char)
  (declare (type string json-control-char)) 
  (assert (eq (aref json-control-char 0) #\\))
  (assert (>= (length json-control-char) 2))
  (ecase (aref json-control-char 1)
    (#\" "\"")
    (#\\ "\\")
    (#\/ "\/")
    (#\b "") ; backspace
    (#\f "") ; formfeed
    (#\n (string #\Newline)) ; newline
    (#\r "") ; carriage return
    (#\t "") ; tab
    (#\u
     (let ((hex-char (register-groups-bind (unicode-char)
                         ("^\\\\u([0-9a-fA-F]{4})$" json-control-char)
                       (string
                        (code-char
                         (read-from-string
                          (concatenate 'string "#x" unicode-char)))))))
       (assert (not (null hex-char)))
       hex-char))))

(defun number-from-json (json-str)
  (register-groups-bind (json-num)
      (*json-number-regex* json-str)
    (assert (not (null json-num)))
    (let ((lisp-num
           (unless (null json-num) (read-from-string json-num))))
      (if (numberp lisp-num) lisp-num nil))))
