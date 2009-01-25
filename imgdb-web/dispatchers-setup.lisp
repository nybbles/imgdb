(in-package :imgdb-web)

(defmethod setup-dispatch-table ((web imgdb-web-server))
  (clear-dispatch-table web)
  (mapcar
   #'(lambda (dispatch)
       (let ((dispatch-type (first dispatch))
             (url (second dispatch))
             (handler-method (third dispatch)))
         (let ((create-dispatcher-fn
                (case dispatch-type
                  (:regex #'create-regex-dispatcher)
                  (:prefix #'create-prefix-dispatcher)
                  (otherwise (error "Invalid dispatcher type"))))
               (dispatcher-fn
                (create-dispatcher-fn-for-handler-method web handler-method)))
           (push
            (funcall create-dispatcher-fn url dispatcher-fn)
            (dispatch-table web)))))
   '((:regex "/.*" 'not-found-page)
     (:prefix "/index.htm" 'welcome-page)
     (:regex "^/$" 'welcome-page)
     (:regex "/img-urls/thumbnails/[a-f0-9]+$" 'img-thumbnail-handler)
     (:regex "/img-urls/[a-f0-9]+$" 'img-url-handler)
     (:regex "/img-view$" 'img-view-page)
     (:regex "/img-query$" 'img-query-page)
     (:regex "/get-img-tags$" 'get-img-tags-handler)
     (:regex "/add-img-tags$" 'add-img-tags-handler)
     (:regex "/delete-img-tags$" 'delete-img-tags-handler)
     (:regex "/set-img-title$" 'set-img-title-handler)
     (:regex "/set-img-description$" 'set-img-description-handler)))
  (push
   (create-folder-dispatcher-and-handler
    "/js/"
    (merge-pathnames "js/" (web-root web)))
   (dispatch-table web))
  (push
   (create-static-file-dispatcher-and-handler
    "/css/imgdb-web.css"
    (merge-pathnames "css/imgdb-web.css" (web-root web)))
   (dispatch-table web)))

(defmethod create-dispatcher-fn-for-method
    ((web imgdb-web-server) dispatch-method)
  #'(lambda () (funcall dispatch-method web)))
(defmethod clear-dispatch-table ((web imgdb-web-server))
  (setf (dispatch-table web) '()))

