
;;;; game-launcher-api.lisp -- REST adapter for the game-launcher framework.
;;;;
;;;; Three layers, kept deliberately separate:
;;;;   1. JSON plumbing (errors, responses, body parsing)
;;;;   2. An adapter that only deals in game-launcher objects
;;;;   3. Routes (versioned /api/v1 plus legacy aliases)

(in-package #:game-launcher-api)

(defvar *api-version* "1.0.0"
  "Reported by /api/v1/health.")

(defvar *thumbnails* t
  "Persisted UI preference: may frontends fetch thumbnail images?")

(defvar *import-sessions* (make-hash-table :test #'equal)
  "id -> list of unregistered candidate launchers.")

(defvar *import-session-counter* 0)

;;; ---------------------------------------------------------------------
;;; JSON plumbing
;;;
;;; cl-json's guessing encoder turns NIL values into arrays, which makes
;;; sparse objects (profile, exec, ...) ambiguous. Responses use a small
;;; deterministic encoder instead; cl-json is still used for decoding.
;;; NIL encodes as null, T as true, :FALSE as false, vectors as arrays and
;;; alists as objects.
;;; ---------------------------------------------------------------------

(defmacro obj (&rest pairs)
  `(list ,@(loop for (k v) on pairs by #'cddr
                 collect `(cons ,k ,v))))

(defun arr (list) (coerce list 'vector))
(defun bool (value) (if value t :false))

(defun json-string (string)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across (princ-to-string string)
          do (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (#\Page (write-string "\\f" out))
               (#\Backspace (write-string "\\b" out))
               (t (if (< (char-code ch) 32)
                      (format out "\\u~4,'0X" (char-code ch))
                      (write-char ch out)))))
    (write-char #\" out)))

(defun json-key (key)
  (json-string (if (symbolp key)
                   (string-downcase (symbol-name key))
                   (princ-to-string key))))

(defun json-encode (value)
  (cond
    ((null value) "null")
    ((eq value t) "true")
    ((eq value :false) "false")
    ((stringp value) (json-string value))
    ((numberp value) (princ-to-string value))
    ((vectorp value)
     (format nil "[~{~A~^,~}]" (map 'list #'json-encode value)))
    ((and (consp value) (consp (car value)))
     (format nil "{~{~A~^,~}}"
             (loop for (key . val) in value
                   collect (format nil "~A:~A" (json-key key)
                                   (json-encode val)))))
    ((consp value)
     (format nil "[~{~A~^,~}]" (mapcar #'json-encode value)))
    ((symbolp value) (json-string (string-downcase (symbol-name value))))
    (t (json-string (princ-to-string value)))))

(defun json-response (object &key (status 200))
  (setf (content-type*) "application/json")
  (setf (return-code*) status)
  (json-encode object))

;;; ---------------------------------------------------------------------
;;; Settings (store-keyed, persisted under XDG data home)
;;; ---------------------------------------------------------------------

(defun settings-path ()
  (uiop:xdg-data-home "game-launcher-api/settings.lisp"))

(defun path-string (path)
  (etypecase path
    (null "")
    (string path)
    (pathname (namestring path))
    (t (princ-to-string path))))

(defun settings-key (&optional store)
  (path-string (or store (game-launcher:store-path) "default")))

(defun read-settings-entries ()
  "Read the store-keyed settings alist. Returns NIL for a missing/broken file."
  (when (probe-file (settings-path))
    (handler-case
        (with-open-file (stream (settings-path) :direction :input)
          (let ((*package* (find-package :game-launcher-api))
                (*read-eval* nil))
            (let ((forms (read stream nil '())))
              (if (and (listp forms) (every #'consp forms)) forms '()))))
      (error (e)
        (warn "game-launcher-api: ignoring unreadable settings ~A: ~A"
              (settings-path) e)
        '()))))

(defun settings-entry (entries key)
  (cdr (assoc key entries :test #'string=)))

(defun current-settings-entry ()
  (settings-entry (read-settings-entries) (settings-key)))

(defun apply-persisted-ui-settings! ()
  "Apply runtime path and thumbnail toggle for the current store."
  (let ((entry (current-settings-entry)))
    (let ((runtime (getf entry :runtime-path)))
      (when (and runtime (plusp (length runtime)) (probe-file runtime))
        (handler-case
            (game-launcher:runtime-path :path (uiop:ensure-directory-pathname runtime))
          (error (e)
            (warn "game-launcher-api: bad persisted runtime path ~A: ~A" runtime e)))))
    (let ((thumbs (getf entry :thumbnails)))
      (when (member thumbs '(t nil))
        (setf *thumbnails* thumbs)))))

(defun write-settings-entry! (&key store runtime thumbnails thumbnails-p)
  "Persist settings for STORE (default: current store).

Keys that are not supplied keep their previously persisted value.  In
particular a store or runtime change must not reset the thumbnail toggle,
which is what happened when this always wrote (IF THUMBNAILS T NIL)."
  (let* ((key (settings-key store))
         (entries (remove-if (lambda (e) (string= (car e) key))
                             (read-settings-entries)))
         (previous (cdr (assoc key entries :test #'string=)))
         (previous-thumbnails-p (not (null (get-properties previous
                                                            '(:thumbnails)))))
         (entry
           (list key
                 :store (path-string (or store (game-launcher:store-path)))
                 :runtime-path (path-string
                                (or runtime
                                    (getf previous :runtime-path)
                                    (game-launcher:runtime-path)))
                 :thumbnails
                 (cond
                   (thumbnails-p (and thumbnails t))
                   (previous-thumbnails-p (and (getf previous :thumbnails) t))
                   (t *thumbnails*)))))
    (ensure-directories-exist (settings-path))
    (with-open-file (stream (settings-path) :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
      (let ((*package* (find-package :game-launcher-api))
            (*print-length* nil)
            (*print-level* nil))
        (pprint (append entries (list entry)) stream)
        (terpri stream)))
    entry))

(defun settings-payload ()
  (obj :store (path-string (game-launcher:store-path))
       :runtime-path (path-string (game-launcher:runtime-path))
       :thumbnails (bool *thumbnails*)
       :file (path-string (settings-path))))

;; Load persisted UI settings and initialize the launcher registry once.
(apply-persisted-ui-settings!)
(game-launcher:initialize-launcher)

;;; ---------------------------------------------------------------------
;;; JSON plumbing
;;; ---------------------------------------------------------------------

(defun status->code (status)
  (case status
    (400 "bad_request")
    (404 "not_found")
    (405 "method_not_allowed")
    (409 "conflict")
    (502 "bad_gateway")
    (t "error")))

(defun json-error (status message)
  (json-response
   (obj :error (obj :code (status->code status) :message message))
   :status status))

(define-condition api-error (error)
  ((status  :initarg :status  :reader api-error-status)
   (message :initarg :message :reader api-error-message)))

(defun api-error (status control &rest args)
  (error 'api-error :status status
                :message (apply #'format nil control args)))

(defmacro with-json-errors (&body body)
  `(handler-case
       (progn ,@body)
     (api-error (e)
       (json-error (api-error-status e) (api-error-message e)))
     (game-launcher:unknown-runtime (e)
       (json-error 400 (princ-to-string e)))
     (reader-error (e)
       (json-error 400 (format nil "could not read form: ~A" e)))
     (error (e)
       (json-error 500 (princ-to-string e)))))

(defun check-method (&rest allowed)
  (unless (member (request-method*) allowed)
    (setf (header-out "Allow")
          (format nil "~{~:(~A~)~^, ~}" allowed))
    (api-error 405 "method not allowed")))

(defun alist->plist (alist)
  (loop for (k . v) in alist
        collect k
        collect v))

(defun parse-json-body ()
  (let ((raw (raw-post-data :force-text t)))
    (when (and raw (plusp (length raw)))
      (let ((cl-json:*json-identifier-name-to-lisp* #'string-upcase))
        (alist->plist (cl-json:decode-json-from-string raw))))))

(defun supplied-p (body key)
  "True when KEY is present in plist BODY (getf has no presence value)."
  (not (null (get-properties body (list key)))))

(defun body-string (body key)
  (let ((value (getf body key)))
    (and (stringp value) value)))

(defun body-boolean (body key)
  "Decode a JSON boolean from BODY.  cl-json returns the :FALSE keyword for
false, which is truthy in Lisp, so NIL and :FALSE must both mean false."
  (let ((value (getf body key)))
    (and value (not (eq value :false)))))

(defun body-list (body key)
  (let ((value (getf body key)))
    (when (listp value) value)))

(defun decode-name (raw)
  (handler-case (hunchentoot:url-decode raw)
    (error () raw)))

(defun find-game! (raw-name)
  (let* ((name (decode-name raw-name))
         (launcher (game-launcher:find-launcher name)))
    (or launcher (api-error 404 "no game with name ~s" name))))

(defun commit-store! ()
  "Flush launcher and profile state to disk. Every mutation calls this."
  (game-launcher:write-all-stores)
  (game-launcher:write-profiles-to-file (game-launcher:env-profile-config-path)))

;;; ---------------------------------------------------------------------
;;; Thumbnails
;;;
;;; Fetching and persistence are delegated to the game-launcher-thumbnails
;;; system, so the TUI and the web app share one XDG cache. Cache misses are
;;; fetched in a worker thread and the image endpoint returns 202 for the
;;; client to retry (the same pattern gl-tui uses for lazy loading).
;;; ---------------------------------------------------------------------

(defun thumbnail-content-type (path)
  (case (and (pathname-type path)
             (intern (string-upcase (pathname-type path)) :keyword))
    (:jpg "image/jpeg")
    (:jpeg "image/jpeg")
    (:png "image/png")
    (:gif "image/gif")
    (:webp "image/webp")
    (:bmp "image/bmp")
    (t "application/octet-stream")))

(defun cached-thumbnail-path (launcher)
  "Existing persistent thumbnail source for LAUNCHER, or NIL. Never fetches."
  (ignore-errors
    (let ((source (game-launcher-thumbnails:fetch-launcher-thumbnail-source
                   launcher :cache t :fetch nil)))
      (and source
           (let ((path (game-launcher-thumbnails:lutris-thumbnail-source-path source)))
             (when (and path (probe-file path)) path))))))

;;; Fetch pipeline
;;;
;;; Requests never fetch inline and never spawn one thread per image.  A
;;; single worker drains a FIFO queue, so a catalogue page showing twenty
;;; games performs one Lutris lookup at a time instead of twenty concurrent
;;; searches.  Request threads enqueue their game and wait on a condition
;;; variable for a bounded time, which means a fetch that is already running
;;; (or queued behind one) still answers the first image request instead of
;;; falling back to client-side polling.

(defparameter *thumbnail-wait-seconds* 2.0
  "How long an image request waits for its queued fetch before returning 202.")

(defparameter *thumbnail-failure-ttl* 600
  "Seconds before a failed lookup is retried.")

(defvar *thumbnail-lock* (bt:make-lock "thumbnail-pipeline"))

(defvar *thumbnail-cv* (bt:make-condition-variable :name "thumbnail-pipeline"))

(defvar *thumbnail-queue* '()
  "FIFO of (KEY LAUNCHER) pairs waiting for the worker.")

(defvar *thumbnail-state* (make-hash-table :test 'equal)
  "KEY -> :QUEUED, :RUNNING, :DONE or :FAILED.")

(defvar *thumbnail-state-time* (make-hash-table :test 'equal)
  "KEY -> universal time of the last settled attempt (for failures).")

(defvar *thumbnail-worker-thread* nil)

(defun thumbnail-failed-recently-p (key)
  "True when KEY last failed within *THUMBNAIL-FAILURE-TTL* seconds."
  (and (eq (gethash key *thumbnail-state*) :failed)
       (let ((at (gethash key *thumbnail-state-time*)))
         (and at (< (- (get-universal-time) at) *thumbnail-failure-ttl*)))))

(defun thumbnail-in-flight-p (key)
  (member (gethash key *thumbnail-state*) '(:queued :running)))

(defun ensure-thumbnail-worker ()
  "Start the fetch worker if it is not running. Call with *THUMBNAIL-LOCK* held."
  (unless (and *thumbnail-worker-thread*
               (bt:thread-alive-p *thumbnail-worker-thread*))
    (setf *thumbnail-worker-thread*
          (bt:make-thread #'thumbnail-worker-loop
                          :name "game-launcher-api thumbnails"))))

(defun enqueue-thumbnail! (launcher key)
  "Queue a fetch for LAUNCHER unless one is in flight or recently failed.
Returns T when a job was queued."
  (bt:with-lock-held (*thumbnail-lock*)
    (cond
      ((thumbnail-in-flight-p key) nil)
      ((thumbnail-failed-recently-p key) nil)
      (t
       (setf *thumbnail-queue* (nconc *thumbnail-queue* (list (list key launcher)))
             (gethash key *thumbnail-state*) :queued)
       (ensure-thumbnail-worker)
       (bt:condition-notify *thumbnail-cv*)
       t))))

(defun thumbnail-worker-loop ()
  "Drain *THUMBNAIL-QUEUE*, one lookup at a time, recording outcomes."
  (loop
    (let ((job (bt:with-lock-held (*thumbnail-lock*)
                 (loop
                   (when *thumbnail-queue*
                     (return (pop *thumbnail-queue*)))
                   (bt:condition-wait *thumbnail-cv* *thumbnail-lock*)))))
      (destructuring-bind (key launcher) job
        (bt:with-lock-held (*thumbnail-lock*)
          (setf (gethash key *thumbnail-state*) :running))
        (let ((ok (handler-case
                      (progn
                        (game-launcher-thumbnails:fetch-launcher-thumbnail-source
                         launcher :cache t :fetch t)
                        t)
                    (error () nil))))
          (bt:with-lock-held (*thumbnail-lock*)
            (setf (gethash key *thumbnail-state*) (if ok :done :failed)
                  (gethash key *thumbnail-state-time*) (get-universal-time))
            (bt:condition-notify *thumbnail-cv*)))))))

(defun wait-for-thumbnail (key timeout)
  "Block until KEY's fetch settles or TIMEOUT seconds elapse."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (bt:with-lock-held (*thumbnail-lock*)
      (loop
        (let ((state (gethash key *thumbnail-state*)))
          (unless (member state '(:queued :running))
            (return nil)))
        (let ((left (- deadline (get-internal-real-time))))
          (when (<= left 0)
            (return nil))
          (bt:condition-wait *thumbnail-cv* *thumbnail-lock*
                             :timeout (/ left
                                         internal-time-units-per-second)))))))

(defun send-thumbnail-file (path)
  "Stream PATH to the client as an image. Returns NIL for the route."
  (setf (content-type*) (thumbnail-content-type path))
  (let ((stream (send-headers)))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
        (loop for count = (read-sequence buffer in)
              until (zerop count)
              do (write-sequence buffer stream :end count))))
    (finish-output stream))
  nil)

(defun api-game-thumbnail (raw-name)
  (with-json-errors
    (check-method :get)
    (let* ((launcher (find-game! raw-name))
           (key (game-launcher-thumbnails:thumbnail-cache-target launcher))
           (path (cached-thumbnail-path launcher)))
      (cond
        (path
         (send-thumbnail-file path))
        ;; Nothing to wait for when the toggle is off: say so instead of
        ;; returning 202 forever and leaving the client polling.
        ((not *thumbnails*)
         (json-response
          (obj :error (obj :code "thumbnails_disabled"
                           :message "thumbnail fetching is disabled in settings"))
          :status 404))
        (t
         (enqueue-thumbnail! launcher key)
         (wait-for-thumbnail key *thumbnail-wait-seconds*)
         (let ((ready (cached-thumbnail-path launcher)))
           (cond
             (ready
              (send-thumbnail-file ready))
             ((thumbnail-failed-recently-p key)
              (json-response
               (obj :error (obj :code "not_found"
                                :message "no thumbnail available for this game"))
               :status 404))
             (t
              (setf (header-out "Retry-After") "1")
              (setf (return-code*) 202)
              ""))))))))

;;; ---------------------------------------------------------------------
;;; Adapter: game-launcher objects -> JSON-safe plists
;;; ---------------------------------------------------------------------

(defun form-string (form)
  (with-output-to-string (stream)
    (let ((*package* (find-package :game-launcher))
          (*print-case* :downcase)
          (*print-pretty* nil)
          (*read-eval* nil))
      (prin1 form stream))))

(defun launcher-tag-list (launcher)
  (let ((tags (ignore-errors (game-launcher:launcher-tags launcher))))
    (cond ((null tags) '())
          ((listp tags) (mapcar #'princ-to-string tags))
          (t (list (princ-to-string tags))))))

(defun profile->plist (profile)
  (when profile
    (obj :name (let ((name (game-launcher:profile-name profile)))
                 (and name (string-downcase (string name))))
         :env (arr (mapcar (lambda (entry)
                             (list (princ-to-string (first entry))
                                   (princ-to-string (second entry))))
                           (game-launcher:profile-env profile))))))

(defun phase->plist (clause)
  (destructuring-bind (op &rest args) clause
    (case op
      (add-before
       (obj :op "add-before"
            :anchor (string-downcase (symbol-name (first args)))
            :name (string-downcase (symbol-name (second args)))
            :body (form-string (third args))))
      (add-after
       (obj :op "add-after"
            :anchor (string-downcase (symbol-name (first args)))
            :name (string-downcase (symbol-name (second args)))
            :body (form-string (third args))))
      (replace
       (obj :op "replace"
            :name (string-downcase (symbol-name (first args)))
            :body (form-string (second args))))
      (delete
       (obj :op "delete"
            :name (string-downcase (symbol-name (first args)))))
      (otherwise
       (obj :op (string-downcase (symbol-name op)) :args args)))))

(defun phases->plist (launcher)
  (let ((phases (ignore-errors (game-launcher:launcher-phases launcher))))
    (when (listp phases)
      (mapcar #'phase->plist phases))))

(defun launcher->api-plist (launcher &key detail)
  "The single serialization used by every route. DETAIL adds the heavier
   fields that are only useful on the per-game endpoint."
  (let ((base
          (obj :name (game-launcher:launcher-name launcher)
               :runtime (string-downcase
                         (string (game-launcher:launcher-runtime launcher)))
               :tags (arr (launcher-tag-list launcher))
               :running (bool (game-launcher:launcher-running-p launcher)))))
    (if detail
        (append base
                (obj :exec (game-launcher:launcher-exec launcher)
                     :args (let ((args (ignore-errors
                                        (game-launcher:launcher-args launcher))))
                             (cond ((stringp args) args)
                                   ((listp args) (arr args))
                                   (t nil)))
                     :profile (profile->plist (game-launcher:launcher-profile launcher))
                     :phases (arr (or (phases->plist launcher) '()))
                     :form (form-string (game-launcher:launcher->form launcher))
                     :log-path (path-string
                                (game-launcher:resolve-launcher-log-path launcher))))
        base)))

;;; ---------------------------------------------------------------------
;;; Log helpers (session logs are written by game-launcher's launch pipeline)
;;; ---------------------------------------------------------------------

(defun sanitize-log-text (text)
  "Strip ANSI/OSC sequences and control characters; normalize line endings.
   Implemented as a small state machine so no regex quoting is needed."
  (with-output-to-string (out)
    (let ((state :text))
      (loop for ch across (or text "")
            do (case state
                 (:text
                  (cond
                    ((char= ch #\Escape) (setf state :esc))
                    ((char= ch #\Return) (write-char #\Newline out))
                    ((and (or (<= (char-code ch) 8)
                              (member (char-code ch) '(11 12 14 15 16 17 18 19
                                                        20 21 22 23 24 25 26 27
                                                        28 29 30 31 127)))
                          (not (char= ch #\Tab)))
                     nil)
                    (t (write-char ch out))))
                 (:esc
                  (case ch
                    (#\[ (setf state :csi))
                    (#\] (setf state :osc))
                    (otherwise (setf state :text))))
                 (:csi
                  (when (and (>= (char-code ch) #x40)
                             (<= (char-code ch) #x7E))
                    (setf state :text)))
                 (:osc
                  (cond
                    ((char= ch #\Bell) (setf state :text))
                    ((char= ch #\Escape) (setf state :osc-esc))
                    ((char= ch #\Newline) (setf state :text))))
                 (:osc-esc
                  (if (char= ch #\\)
                      (setf state :text)
                      (setf state :osc))))))))

(defun log-text (launcher)
  (let ((path (game-launcher:resolve-launcher-log-path launcher)))
    (values
     (when (and path (probe-file path))
       (sanitize-log-text (uiop:read-file-string path)))
     path)))

(defun log-tail (launcher &key (chars 65536))
  (multiple-value-bind (text path) (log-text launcher)
    (values
     (when text
       (let ((len (length text)))
         (subseq text (max 0 (- len chars)) len)))
     path)))

;;; ---------------------------------------------------------------------
;;; Edit / validation helpers
;;; ---------------------------------------------------------------------

(defun all-pipeline-anchors ()
  "Union of anchor names across every *...-pipeline* parameter in the
   game-launcher package. Used to validate phase clauses generically."
  (let (anchors)
    (let ((pkg (find-package :game-launcher)))
      (do-symbols (sym pkg)
        (when (and (boundp sym)
                   (search "-PIPELINE" (symbol-name sym))
                   (listp (symbol-value sym)))
          (dolist (phase (symbol-value sym))
            (pushnew (car phase) anchors)))))
    (or anchors '(pre exec post teardown))))

(defun clause-from-json (c)
  (let* ((op (intern (string-upcase (or (getf c :op) ""))
                     :game-launcher))
         (args (list (getf c :anchor) (getf c :name) (getf c :body))))
    (case op
      (add-before (list op (first args) (second args) (third args)))
      (add-after  (list op (first args) (second args) (third args)))
      (replace    (list op (second args) (third args)))
      (delete     (list op (second args)))
      (otherwise (api-error 400 "unknown phase operation ~A" (getf c :op))))))

(defun validate-clauses (clauses)
  (handler-case
      (progn
        (game-launcher:apply-phase-clauses
         (mapcar (lambda (a) (cons a nil)) (all-pipeline-anchors))
         clauses)
        clauses)
    (error (e)
      (api-error 400 "invalid phase clause: ~A" e))))

(defun profile-from-json (spec)
  (cond
    ((null spec) (game-launcher:resolve-profile nil))
    ((stringp spec)
     (let ((name (intern (string-upcase spec) :game-launcher)))
       (unless (gethash name game-launcher:*named-profiles*)
         (api-error 400 "unknown profile ~s" spec))
       (game-launcher:resolve-profile name)))
    ((listp spec)
     (let ((entries (mapcar (lambda (pair)
                              (list (princ-to-string (first pair))
                                    (princ-to-string (second pair))))
                            spec)))
       (handler-case (game-launcher:make-profile entries)
         (error (e) (api-error 400 "invalid profile environment: ~A" e)))))
    (t (api-error 400 "profile must be a name or an environment list"))))

(defun apply-edits! (launcher body)
  "Validate BODY and mutate LAUNCHER in place (preserving provenance)."
  (let ((name-supplied (supplied-p body :name))
        (exec-supplied (supplied-p body :exec))
        (tags-supplied (supplied-p body :tags))
        (phases-supplied (supplied-p body :phases))
        (profile-supplied (supplied-p body :profile)))
    (declare (ignore name-supplied))
    ;; Validate everything before mutating anything.
    (let* ((new-name (getf body :name))
           (new-exec (getf body :exec))
           (new-tags (body-list body :tags))
           (new-phases (body-list body :phases))
           (new-profile (getf body :profile)))
      (when (and new-name (stringp new-name))
        (when (or (zerop (length (string-trim " " new-name)))
                  (find-if (lambda (other)
                             (and (not (eq other launcher))
                                  (string= new-name
                                           (game-launcher:launcher-name other))))
                           (game-launcher:launcher-list)))
          (api-error 409 "a launcher named ~s already exists" new-name)))
      (when (and new-exec (not (stringp new-exec)))
        (api-error 400 "exec must be a string"))
      (when (and tags-supplied new-tags
                 (not (every #'stringp new-tags)))
        (api-error 400 "tags must be a list of strings"))
      (let ((clauses (when phases-supplied
                       (mapcar #'clause-from-json (or new-phases '())))))
        (when clauses (validate-clauses clauses))
        (let ((profile (when profile-supplied
                         (profile-from-json new-profile))))
          ;; All validation passed; mutate.
          (when (and new-name (stringp new-name))
            (game-launcher:rename-launcher launcher new-name))
          (when (and exec-supplied (stringp new-exec))
            (setf (game-launcher:launcher-exec launcher) new-exec))
          (when tags-supplied
            (setf (game-launcher:launcher-tags launcher) (or new-tags '())))
          (when phases-supplied
            (setf (game-launcher:launcher-phases launcher) (or clauses '())))
          (when profile-supplied
            (setf (game-launcher:launcher-profile launcher) profile))
          (commit-store!)
          launcher)))))

;;; ---------------------------------------------------------------------
;;; Import sessions
;;; ---------------------------------------------------------------------

(defun extension-symbol (package name)
  (let ((pkg (find-package package)))
    (and pkg
         (multiple-value-bind (sym status) (find-symbol (string-upcase name) pkg)
           (declare (ignore status))
           (and sym (fboundp sym) sym)))))

(defun call-extension (package name &rest args)
  (let ((fn (extension-symbol package name)))
    (unless fn (error "~A:~A is not loaded" package name))
    (apply fn args)))

(defun parse-launcher-form (text)
  (let ((*read-eval* nil)
        (*package* (find-package :game-launcher)))
    (read-from-string text)))

(defun make-candidate (runtime &rest args)
  (apply #'game-launcher:make-launcher
         (intern (string-upcase runtime) :game-launcher) args))

(defun steam-local-candidates ()
  "Scan local Steam libraries into unregistered candidates.
   A single stale library entry must not abort the whole scan."
  (let ((vdf (uiop:xdg-data-home "Steam/steamapps/libraryfolders.vdf")))
    (unless (probe-file vdf)
      (return-from steam-local-candidates '()))
    (let* ((parsed (handler-case
                       (call-extension :game-launcher.steam "parse-acf" vdf)
                       (error () nil)))
           (folders (and parsed
                         (handler-case
                             (call-extension :game-launcher.steam "acf-ref"
                                             parsed "libraryfolders")
                           (error () nil)))))
      (loop for folder-entry in folders
            for index = (car folder-entry)
            when (and (stringp index) (every #'digit-char-p index))
              append
              (let ((entry (cdr folder-entry))
                    (root (handler-case
                              (call-extension :game-launcher.steam "acf-ref"
                                              (cdr folder-entry) "path")
                            (error () nil)))
                    (apps (or (handler-case
                                  (call-extension :game-launcher.steam "acf-ref"
                                                  (cdr folder-entry) "apps")
                                (error () nil))
                              '())))
                (when root
                  (loop for app in apps
                        for candidate =
                        (handler-case
                            (call-extension :game-launcher.steam "steam-local-import"
                                            (car app) root :dry t)
                          (error () nil))
                        when candidate collect candidate)))))))

(defun discover-candidates (source body)
  (case (intern (string-upcase (or source "")) :keyword)
    (:form
     (let ((form (parse-launcher-form (or (body-string body :form) ""))))
       (unless (and (consp form) (symbolp (car form)))
         (api-error 400 "not a launcher form"))
       (list (apply #'game-launcher:make-launcher form))))
    (:manual
     (let ((runtime (body-string body :runtime))
           (name (body-string body :name))
           (exec (body-string body :exec))
           (args (body-string body :args))
           (tags (body-list body :tags)))
       (unless (and runtime name exec)
         (api-error 400 "manual import needs runtime, name, and exec"))
       (let ((options (list :name name :exec exec)))
         (when (and args (plusp (length args)))
           (setf (getf options :args) args))
         (when tags
           (unless (every #'stringp tags)
             (api-error 400 "tags must be a list of strings"))
           (setf (getf options :tags) tags))
         (list (apply #'make-candidate runtime options)))))
    (:steam-id
     (let* ((raw-id (body-string body :id))
            (id (and raw-id (string-trim '(#\Space #\Tab #\Newline) raw-id))))
       (when (or (null id) (zerop (length id)))
         (api-error 400 "steam-id import needs an id or url"))
       (let* ((app-id
                (or (and (every #'digit-char-p id) (parse-integer id))
                    (ignore-errors
                     (call-extension :game-launcher.steam "steam-app-id-from-url" id))))
              (name
                (and app-id
                     (ignore-errors
                      (call-extension :game-launcher.steam "steam-app-name" app-id)))))
         (unless app-id (api-error 400 "cannot determine a Steam App ID from ~s" id))
         (list (make-candidate "steam"
                               :name (or name (format nil "Steam Game ~D" app-id))
                               :exec (format nil "steam://rungameid/~D" app-id))))))
    (:steam-local (steam-local-candidates))
    (:lutris
     (let ((games (call-extension :game-launcher.lutris "load-lutris-games")))
       (loop for game in games
             when (ignore-errors
                   (call-extension :game-launcher.lutris
                                   "convert-lutris-game-struct" game))
               collect it)))
    (:desktop
     (let ((file (body-string body :file)))
       (unless file (api-error 400 "desktop import needs a file path"))
       (unless (probe-file file)
         (api-error 400 "desktop file not found: ~a" file))
       (let ((launcher
               (handler-case
                   (call-extension :game-launcher.desktop
                                   "import-desktop-file" file :register nil)
                 (error (e)
                   (api-error 400 "could not read desktop file: ~a"
                              (princ-to-string e))))))
         (when launcher (list launcher)))))
    (otherwise (api-error 400 "unknown import source ~s" source))))

(defun candidate->plist (candidate source)
  (append (launcher->api-plist candidate :detail t)
          (obj :imported
               (bool (find (game-launcher:launcher-name candidate)
                                (game-launcher:launcher-list)
                                :key #'game-launcher:launcher-name
                                :test #'string=))
               :source source)))

(defun new-import-session! (candidates)
  (let ((id (format nil "imp-~D" (incf *import-session-counter*))))
    (setf (gethash id *import-sessions*) candidates)
    ;; Keep the table small; sessions are short-lived by nature.
    (when (> (hash-table-count *import-sessions*) 32)
      (clrhash *import-sessions*)
      (setf (gethash id *import-sessions*) candidates))
    id))

;;; ---------------------------------------------------------------------
;;; Routes
;;; ---------------------------------------------------------------------

(define-easy-handler (api-health-v1 :uri "/api/v1/health") ()
  (with-json-errors
    (check-method :get)
    (json-response
     (obj :status "ok"
          :version *api-version*
          :store (path-string (game-launcher:store-path))
          :runtimes (length (game-launcher:known-runtimes))))))

(defun api-health-legacy ()
  (with-json-errors
    (check-method :get)
    (json-response (obj :status "ok"))))

(defun api-games (versioned)
  (with-json-errors
    (check-method :get)
    (let* ((query (or (ignore-errors (parameter "q")) ""))
           (launchers (if (plusp (length query))
                          (game-launcher:search-launcher-list query)
                          (game-launcher:launcher-list)))
           (items (mapcar #'launcher->api-plist launchers)))
      (if versioned
          (json-response (obj :items (arr items)
                              :count (length items)
                              :store (path-string (game-launcher:store-path))))
          (json-response items)))))

(defun api-game (raw-name)
  (with-json-errors
    (check-method :get)
    (json-response (launcher->api-plist (find-game! raw-name) :detail t))))

(defun api-game-status (raw-name)
  (with-json-errors
    (check-method :get)
    (let* ((launcher (find-game! raw-name))
           (running (game-launcher:launcher-running-p launcher))
           (process (and running (game-launcher:launcher-process launcher))))
      (json-response
       (obj :name (game-launcher:launcher-name launcher)
            :running (bool running)
            :pid (and process (ignore-errors (sb-ext:process-pid process))))))))

(defun api-game-launch (raw-name)
  (with-json-errors
    (check-method :post)
    (let ((launcher (find-game! raw-name)))
      (when (game-launcher:launcher-running-p launcher)
        (api-error 409 "~a is already running"
                   (game-launcher:launcher-name launcher)))
      (game-launcher:run-launcher-thread launcher)
      (json-response
       (obj :name (game-launcher:launcher-name launcher)
            :status "launching")
       :status 202))))

(defun api-game-stop (raw-name)
  (with-json-errors
    (check-method :post)
    (let* ((launcher (find-game! raw-name))
           (result (multiple-value-list
                    (game-launcher:stop-launcher launcher))))
      (unless (and result (car result))
        (api-error 404 "~a has no running process"
                   (game-launcher:launcher-name launcher)))
      (json-response
       (obj :name (game-launcher:launcher-name launcher)
            :status "stopped"
            :pid (getf (second result) :pid))))))

(defun api-game-remove (raw-name)
  (with-json-errors
    (check-method :post :delete)
    (let ((launcher (find-game! raw-name)))
      (game-launcher:remove-launcher (game-launcher:launcher-name launcher))
      (commit-store!)
      (json-response
       (obj :name (game-launcher:launcher-name launcher)
            :status "removed")))))

(defun api-game-patch (raw-name)
  (with-json-errors
    (check-method :patch :post)
    (let* ((launcher (find-game! raw-name))
           (body (or (parse-json-body) '()))
           (edited (apply-edits! launcher body)))
      (json-response (launcher->api-plist edited :detail t)))))

(defun api-game-create ()
  (with-json-errors
    (check-method :post)
    (let* ((body (or (parse-json-body) '()))
           (text (body-string body :form)))
      (unless text (api-error 400 "a :form string is required"))
      (let* ((form (parse-launcher-form text))
             (launcher
               (if (and (consp form) (symbolp (car form)))
                   (apply #'game-launcher:make-launcher form)
                   (api-error 400 "not a launcher form")))
             (name (game-launcher:launcher-name launcher)))
        (when (game-launcher:find-launcher name)
          (api-error 409 "a launcher named ~s already exists" name))
        (game-launcher:register-launcher-instance launcher)
        (commit-store!)
        (json-response (launcher->api-plist launcher :detail t) :status 201)))))

(defun api-game-log (raw-name)
  (with-json-errors
    (check-method :get)
    (let* ((launcher (find-game! raw-name))
           (chars (or (ignore-errors
                       (parse-integer (or (parameter "tail") "65536")
                                      :junk-allowed t))
                      65536))
           (text (log-tail launcher :chars chars)))
      (if text
          (json-response (obj :name (game-launcher:launcher-name launcher)
                              :status "ok"
                              :text text))
          (json-response (obj :name (game-launcher:launcher-name launcher)
                              :status "no-log"
                              :text ""
                              :message
                              "this runtime does not capture output"))))))

(defun api-game-log-stream (raw-name)
  "Short long-poll used by the Node SSE bridge. Returns new text from OFFSET,
   waiting briefly for the log to grow so the browser does not need to poll."
  (with-json-errors
    (check-method :get)
    (let* ((launcher (find-game! raw-name))
           (offset (or (ignore-errors
                        (parse-integer (or (parameter "offset") "0")
                                       :junk-allowed t))
                       0))
           (deadline (+ (get-internal-real-time)
                        (* 10 internal-time-units-per-second)))
           (chunk "")
           (text (log-text launcher)))
      (when text
        (loop while (zerop (length chunk))
              while (< (get-internal-real-time) deadline)
              do (let ((len (length text)))
                   (when (> len offset)
                     (setf chunk (subseq text offset len))))
                 (when (zerop (length chunk)) (sleep 0.25))
                 (setf text (or (log-text launcher) ""))))
      (json-response
       (obj :chunk chunk
            :offset (+ offset (length chunk))
            :timeout (zerop (length chunk))
            :status (if text "ok" "no-log"))))))

(defun api-import-candidates ()
  (with-json-errors
    (check-method :post)
    (let* ((body (or (parse-json-body) '()))
           (source (body-string body :source))
           (objects (discover-candidates source body))
           (candidates
             (loop for candidate in objects
                   collect (candidate->plist candidate source)))
           (id (new-import-session! objects)))
      (json-response (obj :session id :candidates (arr candidates)
                          :count (length candidates))))))

(defun api-import-commit ()
  (with-json-errors
    (check-method :post)
    (let* ((body (or (parse-json-body) '()))
           (id (body-string body :session))
           (names (body-list body :names))
           (candidates (and id (gethash id *import-sessions*))))
      (unless candidates (api-error 404 "unknown or expired import session"))
      (let (imported)
        (dolist (name names)
          (let ((candidate (find name candidates
                                 :key #'game-launcher:launcher-name
                                 :test #'string=)))
            (when candidate
              (unless (game-launcher:find-launcher name)
                (game-launcher:register-launcher-instance candidate)
                (push name imported)))))
        (remhash id *import-sessions*)
        (commit-store!)
        (json-response (obj :status "ok" :imported (arr (nreverse imported))))))))

(defun api-runtimes ()
  (with-json-errors
    (check-method :get)
    (json-response
     (obj :runtimes
          (arr (sort (mapcar (lambda (r) (string-downcase (string r)))
                             (game-launcher:known-runtimes))
                     #'string<))
          :runtime-path (path-string (game-launcher:runtime-path))))))

(defun api-stores ()
  (with-json-errors
    (check-method :get)
    (json-response
     (obj :primary (path-string (game-launcher:store-path))
          :paths (arr (mapcar #'path-string (game-launcher:store-paths)))
          :profiles (path-string (game-launcher:env-profile-config-path))))))

(defun api-settings-get ()
  (with-json-errors
    (check-method :get)
    (json-response (settings-payload))))

(defun change-store! (path)
  (let ((resolved (and (plusp (length (or path "")))
                       (ignore-errors (probe-file path)))))
    (unless resolved (api-error 400 "not a valid file: ~a" path))
    (let ((before (path-string (game-launcher:store-path))))
      (if (string= (path-string resolved) before)
          :unchanged
          (progn
            (commit-store!)
            (game-launcher:uninitialize-launcher)
            (game-launcher:store-path (namestring resolved))
            (game-launcher:initialize-launcher)
            (apply-persisted-ui-settings!)
            (write-settings-entry! :store resolved)
            :passed)))))

(defun api-settings-patch ()
  (with-json-errors
    (check-method :patch :post)
    (let* ((body (or (parse-json-body) '()))
           (status :passed))
      (when (supplied-p body :store)
        (setf status (change-store! (or (body-string body :store) ""))))
      (when (supplied-p body :runtime-path)
        (let ((path (body-string body :runtime-path)))
          (when (and path (plusp (length path)))
            (ensure-directories-exist path)
            (game-launcher:runtime-path
             :path (uiop:ensure-directory-pathname path)))))
      (when (supplied-p body :thumbnails)
        (setf *thumbnails* (and (getf body :thumbnails) t)))
      (write-settings-entry!)
      (json-response
       (obj :status (string-downcase (symbol-name status))
            :settings (settings-payload))))))

;; Legacy alias used by the original frontend.
(defun api-setting-set-store ()
  (with-json-errors
    (check-method :post)
    (let* ((body (or (parse-json-body) '()))
           (raw (body-string body :path))
           (before (path-string (game-launcher:store-path)))
           (status (change-store! (or raw ""))))
      (json-response
       (obj :status (string-downcase (symbol-name status))
            :path (path-string (game-launcher:store-path)))))))

;;; ---------------------------------------------------------------------
;;; Dispatcher
;;; ---------------------------------------------------------------------

(defun api-dispatcher (request)
  (let* ((raw (hunchentoot:script-name request))
         (path (subseq raw 0 (or (position #\? raw) (length raw)))))
    (or
     ;; --- versioned API -------------------------------------------------
     (when (string= path "/api/v1/health")
       (lambda () (api-health-v1)))
     (when (string= path "/api/v1/games")
       (lambda ()
         (if (eq (request-method*) :post)
             (api-game-create)
             (api-games t))))
     (when (string= path "/api/v1/import/candidates")
       (lambda () (api-import-candidates)))
     (when (string= path "/api/v1/import/commit")
       (lambda () (api-import-commit)))
     (when (string= path "/api/v1/runtimes")
       (lambda () (api-runtimes)))
     (when (string= path "/api/v1/stores")
       (lambda () (api-stores)))
     (when (string= path "/api/v1/settings")
       (lambda ()
         (if (member (request-method*) '(:patch :post))
             (api-settings-patch)
             (api-settings-get))))
     (register-groups-bind (name) ("^/api/v1/games/([^/]+)/status$" path)
       (lambda () (api-game-status name)))
     (register-groups-bind (name) ("^/api/v1/games/([^/]+)/launch$" path)
       (lambda () (api-game-launch name)))
     (register-groups-bind (name) ("^/api/v1/games/([^/]+)/stop$" path)
       (lambda () (api-game-stop name)))
     (register-groups-bind (name) ("^/api/v1/games/([^/]+)/remove$" path)
       (lambda () (api-game-remove name)))
     (register-groups-bind (name) ("^/api/v1/games/([^/]+)/thumbnail$" path)
       (lambda () (api-game-thumbnail name)))
     (register-groups-bind (name) ("^/api/v1/games/([^/]+)/log/stream$" path)
       (lambda () (api-game-log-stream name)))
     (register-groups-bind (name) ("^/api/v1/games/([^/]+)/log$" path)
       (lambda () (api-game-log name)))
     (register-groups-bind (name) ("^/api/v1/games/([^/]+)$" path)
       (lambda ()
         (case (request-method*)
           (:delete (api-game-remove name))
           (:patch  (api-game-patch name))
           (otherwise (api-game name)))))
     ;; --- legacy aliases ------------------------------------------------
     (when (string= path "/api/health")
       (lambda () (api-health-legacy)))
     (when (string= path "/api/games")
       (lambda () (api-games nil)))
     (when (string= path "/api/set-store")
       (lambda () (api-setting-set-store)))
     (register-groups-bind (name) ("^/api/games/([^/]+)/launch$" path)
       (lambda () (api-game-launch name)))
     (register-groups-bind (name) ("^/api/games/([^/]+)/remove$" path)
       (lambda () (api-game-remove name)))
     (register-groups-bind (name) ("^/api/games/([^/]+)/status$" path)
       (lambda () (api-game-status name)))
     (register-groups-bind (name) ("^/api/games/([^/]+)/thumbnail$" path)
       (lambda () (api-game-thumbnail name)))
     (register-groups-bind (name) ("^/api/games/([^/]+)$" path)
       (lambda () (api-game name))))))

;;; ---------------------------------------------------------------------
;;; Server
;;; ---------------------------------------------------------------------

(defvar *acceptor* nil)

(defun start-server (&key (port 8080) (address "localhost"))
  (setf *dispatch-table*
        (list #'api-dispatcher 'dispatch-easy-handlers))
  (setf *acceptor*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor
                        :port port :address address))))

(defun stop-server ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))
