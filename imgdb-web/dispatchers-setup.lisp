(in-package :imgdb-web)

(defun setup-dispatch-table ()  
  (setf *dispatch-table* '())
  (push (create-regex-dispatcher "/.*" 'not-found-page) *dispatch-table*)
  (push (create-prefix-dispatcher "/index.htm" 'welcome-page) *dispatch-table*)
  (push (create-regex-dispatcher "^/$" 'welcome-page) *dispatch-table*)
  (push (create-regex-dispatcher "/img-urls/thumbnails/[a-f0-9]+$"
                                 'img-thumbnail-handler)
        *dispatch-table*)
  (push (create-regex-dispatcher "/img-urls/[a-f0-9]+$" 'img-url-handler)
        *dispatch-table*)
  (push (create-regex-dispatcher "/img-view$" 'img-view-page)
        *dispatch-table*)
  (push (create-regex-dispatcher "/img-query$" 'img-query-page)
        *dispatch-table*)
  (push (create-folder-dispatcher-and-handler
         "/js/"
         (merge-pathnames "js/" *imgdb-web-root*))
        *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/css/imgdb-web.css"
         (merge-pathnames "css/imgdb-web.css" *imgdb-web-root*))
        *dispatch-table*)
  (push (create-regex-dispatcher "/get-img-tags$" 'get-img-tags-handler)
        *dispatch-table*)
  (push (create-regex-dispatcher "/add-img-tags$" 'add-img-tags-handler)
        *dispatch-table*)
  (push (create-regex-dispatcher "/delete-img-tags$" 'delete-img-tags-handler)
        *dispatch-table*))
