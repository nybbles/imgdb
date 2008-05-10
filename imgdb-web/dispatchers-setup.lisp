(in-package :imgdb-web)

(push (create-regex-dispatcher "/.*" 'not-found-page) *dispatch-table*)

(push (create-prefix-dispatcher "/index.htm" 'welcome-page) *dispatch-table*)
(push (create-regex-dispatcher "^/$" 'welcome-page) *dispatch-table*)

(push (create-prefix-dispatcher "/css/imgdb-web.css" 'imgdb-web-css)
      *dispatch-table*)

(push (create-regex-dispatcher "/img-urls/thumbnails/[a-f0-9]+$"
                                'img-thumbnail-handler)
      *dispatch-table*)
(push (create-regex-dispatcher "/img-urls/[a-f0-9]+$" 'img-url-handler)
      *dispatch-table*)

(push (create-prefix-dispatcher "/img-query" 'img-query-page) *dispatch-table*)
