(in-package :imgdb-web)

(defparameter *imgdb-web-root* nil)

(setf *dispatch-table* '())

(defun not-found-page ()
  (with-html-output-to-string (output nil :prologue t)
    (:html
     (:head (:title "imgdb - 404 (page not found)"))
     (:body (:h2 "Looks like you're lost..")
            "Maybe you could go "
            (:a :href "/" "here") "?"))))

(push (create-regex-dispatcher "/.*" 'not-found-page) *dispatch-table*)
