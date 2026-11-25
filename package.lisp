
(defpackage #:game-launcher-api
  (:use #:cl #:hunchentoot #:cl-json #:cl-ppcre)
  (:import-from #:cl-json #:encode-json-to-string
		#:decode-json-from-string
		#:*json-identifier-name-to-lisp*)
  (:import-from #:bordeaux-threads #:make-thread)
  (:export #:start-server
	   #:stop-server))
