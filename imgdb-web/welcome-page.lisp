(in-package :imgdb-web)

(defun welcome-page ()
  (with-html-output-to-string (output nil :prologue t)
    (:html
     (str (generate-html-head "imgdb"))
     (:body
      (:div :id "front-content"
            (:h1 :align "center" "imgdb")
            (:p
             "It's still under construction, so everything looks terrible."))
      (:div :id "signin"
            (:a :href "login.htm" "Sign in"))))))

(push (create-prefix-dispatcher "/index.htm" 'welcome-page) *dispatch-table*)
(push (create-regex-dispatcher "^/$" 'welcome-page) *dispatch-table*)
