(in-package :imgdb-web)

(defun get-img-tags-handler ()
  (let ((json (from-json (raw-post-data :force-text t))))
    (setf (content-type *reply*) "application/json")
    (comment-filter-json
     (to-json
      (make-json-object
       (with-database (dbconn *imgdb-store-db-conn-spec*
                              :database-type *imgdb-store-db-type*
                              :pool t :if-exists :old)
         (let ((img-id (json-ob-get "imgid" json)))
           (list
            (make-json-object-element "imgid" img-id)
            (make-json-object-element
             "taglist"
             (make-json-array (get-img-tags img-id dbconn)))))))))))

(defun add-img-tags-handler ()
  (let ((json (from-json (raw-post-data :force-text t))))
    (setf (content-type *reply*) "application/json")
    (comment-filter-json
     (to-json
      (make-json-object
       (with-database (dbconn *imgdb-store-db-conn-spec*
                              :database-type *imgdb-store-db-type*
                              :pool t :if-exists :old)
         (let ((img-id (json-ob-get "imgid" json))
               (tags
                (remove-if
                 #'(lambda (x) (scan "^\s*$" x))
                 (get-json-array-contents (json-ob-get "taglist" json)))))
           (list
            (make-json-object-element "imgid" img-id)
            (make-json-object-element
             "taglist"
             (make-json-array (add-img-tags img-id tags dbconn)))))))))))

(defun delete-img-tags-handler ()
  (let ((json (from-json (raw-post-data :force-text t))))
    (setf (content-type *reply*) "application/json")
    (comment-filter-json
     (to-json
      (make-json-object
       (with-database (dbconn *imgdb-store-db-conn-spec*
                              :database-type *imgdb-store-db-type*
                              :pool t :if-exists :old)
         (let ((img-id (json-ob-get "imgid" json))
               (tags (get-json-array-contents (json-ob-get "taglist" json))))
           (list
            (make-json-object-element "imgid" img-id)
            (make-json-object-element
             "taglist"
             (make-json-array (delete-img-tags img-id tags dbconn)))))))))))

(defun make-json-object (elements)
  (cons :object elements))

(defun json-ob-get (key json-ob)
  (unless (and (json-objectp json-ob) (stringp key))
    (error 'json-invalid-type-error))
  (let ((result (assoc key (cdr json-ob) :test #'string=)))
    (if (null result)
        nil
        (second result))))

(defun get-json-object-elements (json-ob)
  (unless (json-objectp json-ob) (error 'json-invalid-type-error))
  (cdr json-ob))

(defun make-json-array (list)
  (cons :array list))

(defun json-aref (index json-array)
  (unless (and (json-arrayp json-array) (typep index '(integer 0)))
    (error 'json-invalid-type-error))
  (let ((result (nthcdr index (cdr json-array))))
    (if (null result)
        nil
        (first result))))

(defun get-json-array-contents (json-array)
  (unless (json-arrayp json-array) (error 'json-invalid-type-error))
  (cdr json-array))

(defun comment-filter-json (json)
  (concatenate 'string "/* " json " */"))

(defun to-json (json-sexp)
  (or (object-to-json json-sexp)
      (error 'json-invalid-sexp-error)))

(defun json-objectp (thing)
  (and (listp thing) (eq (first thing) :object)))

;;; Um, maybe a struct is in order.
(defun json-object-elementp (thing)
  (and (listp thing) (= (length thing) 2) (stringp (first thing))))
(defun json-object-element-key (json-ob-element)
  (first json-ob-element))
(defun json-object-element-value (json-ob-element)
  (second json-ob-element))
(defun make-json-object-element (key value)
  (list key value))

(defun object-to-json (json-sexp)
  (if (json-objectp json-sexp)
      (let ((result (make-array 0 :element-type 'character :fill-pointer t)))
        (with-output-to-string (s result)
          (format s "{")
          (loop for element in (cdr json-sexp)
             with first-element = t
             do
             (unless (json-object-elementp element)
               (error 'json-invalid-sexp-error))
             (unless first-element (format s ", "))
             (let* ((key (string-to-json (first element)))
                    (unparsed-value (second element))
                    (value
                     (or (object-to-json unparsed-value)
                         (array-to-json unparsed-value)
                         (string-to-json unparsed-value)
                         (number-to-json unparsed-value)
                         (null-to-json unparsed-value)
                         (bool-to-json unparsed-value)
                         (error 'json-invalid-sexp-error))))
               (format s "~A : ~A" key value))
             (setf first-element nil))
          (format s "}"))
        result)
      nil))

(defun json-arrayp (thing)
  (and (listp thing) (eq (first thing) :array)))

(defun array-to-json (json-sexp)
  (if (json-arrayp json-sexp)
      (let ((result (make-array 0 :element-type 'character :fill-pointer t)))
        (with-output-to-string (s result)
          (format s "[")
          (loop for element in (cdr json-sexp)
             with first-element = t
             do
             (unless first-element (format s ", "))
             (let ((value
                    (or (object-to-json element)
                        (array-to-json element)
                        (string-to-json element)
                        (number-to-json element)
                        (null-to-json element)
                        (bool-to-json element)
                        (error 'json-invalid-sexp-error))))
               (format s "~A" value))
             (setf first-element nil))
          (format s "]"))
        result)
      nil))

;; Does not do string translations yet.
(defun string-to-json (json-sexp)
  (if (stringp json-sexp) (concatenate 'string "\"" json-sexp "\"") nil))

(defun number-to-json (json-sexp)
  (if (numberp json-sexp) (write-to-string json-sexp) nil))

(defun bool-to-json (json-sexp)
  (if (and (keywordp json-sexp) (member json-sexp '(:true :false)))
      (case json-sexp
        (:true "true")
        (:false "false")
        (t (error 'json-invalid-sexp-error)))
      nil))

(defun null-to-json (json-sexp)
  (if (and (keywordp json-sexp) (eq json-sexp :null)) "null" nil))

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

(defun from-json (json-str)
  (multiple-value-bind (result remaining-str)
      (object-from-json json-str)
    (if (or (null result) (> (length remaining-str) 0))
        (error 'json-parse-error)
        result)))

(defun object-from-json (json-str)
  (let ((json-str (strip-leading-whitespace json-str)))
    (if (and (> (length json-str) 0) (eq (aref json-str 0) #\{))
        (loop with curr-json-str = (subseq json-str 1)
           with results = '()
           do (multiple-value-bind (status token-info rest-str)
                  (next-object-element-from-json curr-json-str (length results))
                (setf curr-json-str rest-str)
                (ecase status
                  (:empty (return (values '(:object) curr-json-str)))
                  (:invalid (return (values nil curr-json-str)))
                  (:element (push token-info results))
                  (:last-element
                   (push token-info results)
                   (return (values (cons :object (reverse results))
                                   curr-json-str))))))
        (values nil json-str))))

(defun array-from-json (json-str)
  (let ((json-str (strip-leading-whitespace json-str)))
    (if (and (> (length json-str) 0) (eq (aref json-str 0) #\[))
        (loop with curr-json-str = (subseq json-str 1)
           with results = '()
           do (multiple-value-bind (status token rest-str)
                  (next-array-element-from-json curr-json-str (length results))
                (setf curr-json-str rest-str)
                (ecase status
                  (:empty (return (values '(:array) curr-json-str)))
                  (:invalid (return (values nil curr-json-str)))
                  (:element (push token results))
                  (:last-element
                   (push token results)
                   (return (values (cons :array (reverse results))
                                   curr-json-str))))))
        (values nil json-str))))

(defmacro next-element-from-json
    (json-str end-delimiter num-results &body token-info-code)
  (let ((token-info (gensym "TOKEN-INFO-"))
        (match-start (gensym "MATCH-START-"))
        (match-end (gensym "MATCH-END-"))
        (token (gensym "TOKEN-"))
        (unprocessed-str (gensym "UNPROCESSED-STR-")))
    `(let ((,token-info
            ;; find the token and its type and return them as token info:
            ;; (LIST token unprocessed-str).
            (progn ,@token-info-code)))
       (if (null ,token-info)
           ;; if a token was not found, check whether it is because there
           ;; are no more tokens left.
           (multiple-value-bind (,match-start ,match-end)
               (scan ,(concatenate 'string "^\\s*" `,end-delimiter) ,json-str)
             (if (and ,match-start ,match-end (= ,num-results 0))
                 (values :empty nil (subseq ,json-str ,match-end))
                 (values :invalid nil ,json-str)))
           ;; if a token was found, check whether it is followed by
           ;; either ',' or the end-delimiter.
           (let ((,token (first ,token-info))
                 (,unprocessed-str (second ,token-info)))
             (multiple-value-bind (,match-start ,match-end)
                 (scan "^\\s*,\\s*" ,unprocessed-str)
               (if (and ,match-start ,match-end)
                   (values :element ,token
                           (subseq ,unprocessed-str ,match-end))
                   (multiple-value-bind (,match-start ,match-end)
                       (scan ,(concatenate 'string 
                                           "^\\s*" `,end-delimiter "\\s*")
                             ,unprocessed-str)
                     (if (and ,match-start ,match-end)
                         (values :last-element ,token
                                 (subseq ,unprocessed-str ,match-end))
                         (values :invalid nil ,json-str))))))))))

(defun next-object-element-from-json (json-str num-results)
  (next-element-from-json json-str "}" num-results
    (get-token-info-for-next-object-element-from-json json-str)))

(defun next-array-element-from-json (json-str num-results)
  (next-element-from-json json-str "]" num-results
    (get-token-info-for-next-array-element-from-json json-str)))

(defun get-token-info-for-next-object-element-from-json (json-str)
  (multiple-value-bind (token-key unprocessed-str)
      (string-from-json json-str)
    (if (null token-key)
        nil
        (multiple-value-bind (match-start match-end)
            (scan "^\\s*:\\s*" unprocessed-str)
          (if (and match-start match-end)
              (let* ((unprocessed-str (subseq unprocessed-str match-end))
                     (partial-token-info
                      (get-token-info-for-next-array-element-from-json
                       unprocessed-str)))
                (when partial-token-info
                  (let ((token (first partial-token-info))
                        (unprocessed-str (second partial-token-info)))
                    (list (list token-key token) unprocessed-str))))
              nil)))))

(defun get-token-info-for-next-array-element-from-json (json-str)
  (dolist (token-type '(:object :array :string :number :bool :null))
    (let
        ((returned-token-info
          (multiple-value-list
           (case token-type
             (:object (object-from-json json-str))
             (:array (array-from-json json-str))
             (:string (string-from-json json-str))
             (:number (number-from-json json-str))
             (:bool (bool-from-json json-str))
             (:null (null-from-json json-str))))))
      (when (not (null (first returned-token-info)))
        (return returned-token-info)))))

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
