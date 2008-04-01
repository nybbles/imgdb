(in-package :imgdb-web)

(defun not-found-page ()
  (with-html-output-to-string (output nil :prologue t)
    (:html
     (str (generate-html-head "imgdb - 404 (page not found)"))
     (:body (:h2 "Looks like you're lost..")
            "Maybe you could go "
            (:a :href "/" "here") "?"))))

(push (create-regex-dispatcher "/.*" 'not-found-page) *dispatch-table*)