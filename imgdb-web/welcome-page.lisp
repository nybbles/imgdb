(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(defun welcome-page ()
  (let ((random-img-url
         (concatenate 'string
                      "img-urls/" (select-random-img-id) "?height=500")))
    (with-html-output-to-string (output nil :prologue t)
      (:html
       (str (generate-html-head "imgdb"))
       (:body
        (:div :id "front-content"
              (:h1 :align "center" "imgdb")
              (:p
               "It's still under construction, so everything looks terrible.")
              (:img :height 500 :src (str random-img-url)))
        (:div :id "signin"
              (:a :href "login.htm" "Sign in")))))))

(defun select-random-img-id ()
  (caar (select-img-records ([digest])
                           :order-by (sql-operation 'function "random" 42)
                           :limit 1 :database *imgdb-dbconn*)))

(push (create-prefix-dispatcher "/index.htm" 'welcome-page) *dispatch-table*)
(push (create-regex-dispatcher "^/$" 'welcome-page) *dispatch-table*)

(restore-sql-reader-syntax-state)
