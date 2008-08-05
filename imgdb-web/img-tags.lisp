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
(defparameter *json-array-regex* "^\\s*[\\s*(.*)\\s*]\\s*")
(defparameter *json-control-char-regex*
  "(\\\\([\\\/\\\\\\\"bfnrt]|u[0-9a-fA-F]{4}))")
(defparameter *json-string-regex*
  (concatenate 'string
               "^\\s*\\\"(([^\\\"\\\\]|"
               *json-control-char-regex*
               ")*)\\\"\\s*"))
(defparameter *json-number-regex*
  "^\\s*(-?(0\\.[0-9]+|[1-9][0-9]*(\\.[0-9]+)?)([eE][+-]?[0-9]+)?)\\s*")
(defparameter *json-bool-regex* "^\\s*(true|false)\\s*")
(defparameter *json-null-regex* "^\\s*(null)\\s*")

(defun from-json (json-str &optional (result nil)))

; Implement non-recursive arrays first. Extending them to be recursive
; will be relatively easy.
(defun array-from-json (json-str)
  (let ((json-str (strip-leading-whitespace json-str)))
    (if (and (> (length json-str) 0) (eq (aref json-str 0) #\[))
        (loop with curr-json-str = (subseq json-str 1)
           with result = '()
           for i from 0 to 9
           do (multiple-value-bind (status token rest-str)
                  (array-element-json curr-json-str)
                (setf curr-json-str rest-str)
                (ecase status
                  (:empty (return (values '(:array) curr-json-str)))
                  (:invalid (return (values nil curr-json-str)))
                  (:element (push token result))
                  (:last-element
                   (push token result)
                   (return (values (cons :array (reverse result))
                                   curr-json-str))))))
        (values nil json-str))))

(defun array-element-json (json-str)
  (let ((token-info
         (dolist (token-type '(:array :string :number :bool :null))
           (let ((returned-token-info 'nil))
             (setf returned-token-info
                   (multiple-value-list
                    (case token-type
                      (:array (array-from-json json-str))
                      (:string (string-from-json json-str))
                      (:number (number-from-json json-str))
                      (:bool (bool-from-json json-str))
                      (:null (null-from-json json-str)))))
             (when (not (null (first returned-token-info)))
               (return returned-token-info))))))
    (if (null token-info)
        (multiple-value-bind (match-start match-end)
            (scan "^\\s*]" json-str)
          (if (and match-start match-end)
              (values :empty nil (subseq json-str match-end))
              (values :invalid nil json-str)))
        ;; strip off extra spaces and trailing ',' or ']'
        (multiple-value-bind (match-start match-end)
            (scan "^\\s*,\\s*" (second token-info))
          (if (and match-start match-end)
              (values :element
                      (first token-info) (subseq (second token-info) match-end))
              (multiple-value-bind (match-start match-end)
                  (scan "^\\s*]\\s*" (second token-info))
                (if (and match-start match-end)
                    (values :last-element
                            (first token-info)
                            (subseq (second token-info) match-end))
                    (values :invalid nil json-str))))))))

(defun string-from-json (json-str &key (convert-control-chars nil))
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan *json-string-regex* json-str)
    (if (and match-start match-end reg-starts reg-ends)
        (let ((found-json-str
               (subseq json-str (aref reg-starts 0) (aref reg-ends 0)))
              (unmatched-json-str (subseq json-str match-end)))
          (values
           (if convert-control-chars
               (convert-json-control-chars-in-string-to-lisp found-json-str)
               found-json-str)
           unmatched-json-str))
        (values nil json-str))))

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
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan *json-number-regex* json-str)
    (if (and match-start match-end reg-starts reg-ends)
        (let ((json-num
               (subseq json-str (aref reg-starts 0) (aref reg-ends 0)))
              (unmatched-json-str (subseq json-str match-end)))
          (let ((lisp-num (read-from-string json-num)))
            (values lisp-num unmatched-json-str)))
        (values nil json-str))))

(defun bool-from-json (json-str)
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan *json-bool-regex* json-str)
    (if (and match-start match-end reg-starts reg-ends)        
        (let ((json-bool
               (subseq json-str (aref reg-starts 0) (aref reg-ends 0)))
              (unmatched-json-str (subseq json-str match-end)))
          (values
           (cond
             ((equal json-bool "true") :true)
             ((equal json-bool "false") :false)
             (t (error 'invalid-bool-value)))
           unmatched-json-str))
        (values nil json-str))))

(defun null-from-json (json-str)
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan *json-null-regex* json-str)
    (if (and match-start match-end reg-starts reg-ends)
        (values :null (subseq json-str match-end))
        (values nil json-str))))

(defun strip-leading-whitespace (str)
  (regex-replace "^\\s+" str ""))
