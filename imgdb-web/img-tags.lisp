(in-package :imgdb-web)

(defun add-img-tags-handler ()
  (let ((json (raw-post-data :force-text t)))
    (setf (content-type *reply*) "application/json")
    (setf (log-file) "/Users/nimalan/Desktop/imgdb-log")
    (log-message* "add-img-tags-handler received post data: [~A]~%"
                  json)
    "/* {\"tags\" : [\"blah\", \"zah\"]} */"))

(defun delete-img-tags-handler ())
