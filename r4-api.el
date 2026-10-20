;;; r4-api.el --- Radio4000 Supabase client  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A small asynchronous client for the Radio4000 Supabase REST API
;; (PostgREST), built on the built-in `url' library.  It mirrors the
;; read queries of the @radio4000/sdk JavaScript package.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'url-http)
(require 'r4-models)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defcustom r4-api-url "https://oupgudlkspsmzkmeovlh.supabase.co"
  "Base URL of the Radio4000 Supabase project."
  :type 'string
  :group 'r4)

(defcustom r4-api-key
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Im91cGd1ZGxrc3BzbXprbWVvdmxoIiwicm9sZSI6ImFub24iLCJpYXQiOjE3MTM0NTIwNzQsImV4cCI6MjAyOTAyODA3NH0.KAbKFBChJHtxTmOZM2pdeppIyNbcnQkEgSi6RA7OQdo"
  "Public (anon) API key of the Radio4000 Supabase project."
  :type 'string
  :group 'r4)

(defcustom r4-api-tracks-limit 5000
  "Maximum number of tracks fetched for one channel."
  :type 'natnum
  :group 'r4)

(defcustom r4-channels-min-tracks 1
  "Only list channels with at least this many tracks.
The default hides the many empty channels."
  :type 'natnum
  :group 'r4)

(defcustom r4-api-search-limit 100
  "Maximum number of results for a search."
  :type 'natnum
  :group 'r4)

(defconst r4-api--channel-columns
  "id,slug,name,description,url,image,track_count,latest_track_at,created_at,updated_at"
  "Columns selected from the `channels_with_tracks' view.")

(defconst r4-api--track-columns
  "id,slug,title,description,url,tags,duration,provider,media_id,discogs_url,created_at,updated_at"
  "Columns selected from the `channel_tracks' view.")

(defun r4-api--url (table params)
  "Return the REST URL for TABLE with query PARAMS, a list of (KEY VALUE)."
  (concat r4-api-url "/rest/v1/" table "?" (url-build-query-string params)))

(defun r4-api--headers ()
  "Return the HTTP headers for an API request."
  `(("apikey" . ,r4-api-key)
    ("Accept" . "application/json")))

(defun r4-api--parse ()
  "Parse the JSON body of the current `url-retrieve' buffer."
  (goto-char url-http-end-of-headers)
  (json-parse-buffer :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(define-error 'r4-api-error "Radio4000 API error")

(defun r4-api--body-message ()
  "Return the error message of the JSON body of the current response, or nil."
  (ignore-errors
    (let ((body (r4-api--parse)))
      (when (consp body)
        (let-alist body (or .message .msg .error_description .error))))))

(defun r4-api--error-message (status)
  "Return a readable error message from the `url-retrieve' STATUS and body."
  (format "Radio4000 API error: %s" (or (r4-api--body-message) (plist-get status :error))))

(defun r4-api-get (table params callback)
  "Fetch rows of TABLE with PARAMS and call CALLBACK with the parsed rows.
On failure, report the error with `message'."
  (let ((url-request-method "GET")
        (url-request-extra-headers (r4-api--headers)))
    (url-retrieve
     (r4-api--url table params)
     (lambda (status)
       (let ((buffer (current-buffer))
             (result (if (plist-get status :error)
                         (list :error (r4-api--error-message status))
                       (condition-case err
                           (list :data (r4-api--parse))
                         (error (list :error (error-message-string err)))))))
         (kill-buffer buffer)
         (if-let* ((error-message (plist-get result :error)))
             (message "%s" error-message)
           (funcall callback (plist-get result :data)))))
     nil t t)))

(cl-defun r4-api-request (method path &key params body token prefer)
  "Send a METHOD request to PATH below `r4-api-url', and return the parsed JSON.
PARAMS is a list of (KEY VALUE) query params, BODY is sent as JSON,
TOKEN is a user access token and PREFER the PostgREST Prefer header.
Block Emacs until the response, and signal `r4-api-error' on failure."
  (let* ((url-request-method method)
         (url-request-noninteractive t)
         (url-request-extra-headers
          `(,@(r4-api--headers)
            ,@(when body '(("Content-Type" . "application/json")))
            ,@(when token `(("Authorization" . ,(concat "Bearer " token))))
            ,@(when prefer `(("Prefer" . ,prefer)))))
         (url-request-data (and body (json-serialize body)))
         (url (concat r4-api-url path (when params (concat "?" (url-build-query-string params)))))
         (buffer (or (url-retrieve-synchronously url t t 60)
                     (signal 'r4-api-error (list "No response from the server")))))
    (with-current-buffer buffer
      (unwind-protect
          (if (and (numberp url-http-response-status)
                   (<= 200 url-http-response-status 299))
              (progn
                (goto-char url-http-end-of-headers)
                (skip-chars-forward " \t\r\n")
                (unless (eobp) (r4-api--parse)))
            (signal 'r4-api-error
                    (list (or (r4-api--body-message)
                              (format "HTTP status %s" url-http-response-status)))))
        (kill-buffer buffer)))))

(defun r4-api-get-sync (table params)
  "Fetch and return the rows of TABLE with PARAMS, blocking Emacs."
  (r4-api-request "GET" (concat "/rest/v1/" table) :params params))

(defun r4-api-search-filter (query)
  "Return the PostgREST `or' filter matching QUERY, or nil if empty.
Like radio4000.com, match QUERY as a web search, with operators such
as \"quotes\", or and -, and also each of its words as a prefix, so
\"osk\" matches \"oskar\"."
  (let ((safe (string-trim (replace-regexp-in-string
                            "[ \t\n]+" " "
                            (replace-regexp-in-string "[,()&|!*\"]" " " query)))))
    (unless (string-empty-p safe)
      (let ((words (unless (string-match-p "\\_<or\\_>\\|\\`-\\|[ ]-" (downcase safe))
                     (delete "" (mapcar (lambda (word)
                                          (replace-regexp-in-string "[^[:alnum:]]" "" word))
                                        (split-string safe " "))))))
        (format "(fts.wfts.%s%s)" safe
                (if words
                    (concat ",fts.fts." (mapconcat (lambda (word) (concat word ":*")) words " & "))
                  ""))))))

(defun r4-api--channels-params (&optional query)
  "Return the query params to list channels, optionally matching QUERY."
  `(("select" ,r4-api--channel-columns)
    ,@(when-let* ((filter (and query (r4-api-search-filter query))))
        `(("or" ,filter)))
    ,@(when (> r4-channels-min-tracks 0)
        `(("track_count" ,(format "gte.%d" r4-channels-min-tracks))))
    ("order" "latest_track_at.desc.nullslast")))

(defun r4-api-channels (callback &optional query)
  "Call CALLBACK with a list of `r4-channel', most recently active first.
If QUERY is non-nil, only return channels matching that full-text search."
  (r4-api-get "channels_with_tracks" (r4-api--channels-params query)
              (lambda (rows) (funcall callback (mapcar #'r4-channel-from-json rows)))))

(defun r4-api-channels-sync ()
  "Return a list of all `r4-channel', most recently active first."
  (mapcar #'r4-channel-from-json
          (r4-api-get-sync "channels_with_tracks" (r4-api--channels-params))))

(defun r4-api-channel (slug callback)
  "Call CALLBACK with the `r4-channel' SLUG, or nil if it does not exist."
  (r4-api-get "channels_with_tracks"
              `(("select" ,r4-api--channel-columns)
                ("slug" ,(concat "eq." slug)))
              (lambda (rows)
                (funcall callback (and rows (r4-channel-from-json (car rows)))))))

(defun r4-api-tracks (callback &rest filters)
  "Call CALLBACK with a list of `r4-track', newest first.
FILTERS is a plist with the optional keys :slug, a channel slug, and
:query, a full-text search."
  (let ((slug (plist-get filters :slug))
        (query (plist-get filters :query)))
    (r4-api-get "channel_tracks"
                `(("select" ,r4-api--track-columns)
                  ,@(when slug `(("slug" ,(concat "eq." slug))))
                  ,@(when-let* ((filter (and query (r4-api-search-filter query))))
                      `(("or" ,filter)))
                  ("order" "created_at.desc")
                  ("limit" ,(number-to-string
                             (if (and query (not slug))
                                 r4-api-search-limit
                               r4-api-tracks-limit))))
                (lambda (rows) (funcall callback (mapcar #'r4-track-from-json rows))))))

(provide 'r4-api)

;;; r4-api.el ends here
