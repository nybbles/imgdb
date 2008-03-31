(in-package :imgdb-web)

(defmacro generate-html-head (title &rest rest)
  `(with-html-output-to-string (,(gensym) nil)
     (:head
      (:link :rel "stylesheet" :type "text/css"
             :href "css/imgdb-web.css")
      (:title ,title)
      ,@rest)))
