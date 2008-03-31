(in-package :imgdb-web)

(defun welcome-page ()
  (with-html-output-to-string (output nil :prologue t)
    (:html
     (:head (:link :rel "stylesheet" :type "text/css"
                   :href "css/imgdb-web.css")
            (:title "imgdb"))
     (:body (:h1 :align "center" "Welcome to imgdb")))))

(push (create-prefix-dispatcher "/index.htm" 'welcome-page) *dispatch-table*)
(push (create-regex-dispatcher "^/$" 'welcome-page) *dispatch-table*)
