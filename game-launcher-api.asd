
(asdf:defsystem #:game-launcher-api
  :description "rest api for game-launcher"
  :author "Rush"
  :license  "GPLv3"
  :version "0.0.6"
  :serial t
  :depends-on (#:hunchentoot
	       #:game-launcher
	       #:cl-json
	       #:cl-ppcre
	       #:bordeaux-threads
	       ;; Shared thumbnail lookup/cache (no TUI dependency).
	       #:game-launcher-thumbnails)
  :components ((:file "package")
               (:file "game-launcher-api")))
