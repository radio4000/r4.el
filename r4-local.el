;;; r4-local.el --- Local-first Radio4000 channels  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Local channels work without a radio4000.com account.  Each one is a
;; Radio4000 backup file (version 2, the format of the "backup" and
;; "import" features of radio4000.com) stored next to the channel
;; downloads, in `r4-download-directory'/SLUG/SLUG.json.
;;
;; Create a local channel, add tracks to it by URL or from any track
;; list, or save a copy of a remote channel to browse it offline.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'r4-models)
(require 'r4-api)
(require 'r4-files)
(require 'r4-download)
(require 'r4-tracks)
(require 'r4-channels)

;;;; Files

(defun r4-local-file (slug)
  "Return the backup file of the local channel SLUG."
  (expand-file-name (concat slug ".json") (r4-files-channel-directory slug)))

(defun r4-local-p (slug)
  "Return non-nil if SLUG is a local channel."
  (file-exists-p (r4-local-file slug)))

(defun r4-local-slugs ()
  "Return the slugs of the local channels."
  (let ((directory (expand-file-name r4-download-directory)))
    (when (file-directory-p directory)
      (cl-loop for slug in (directory-files directory nil "\\`[^.]")
               when (r4-local-p slug) collect slug))))

(defun r4-local-now ()
  "Return the current time as an ISO 8601 UTC timestamp."
  (format-time-string "%FT%T.%3NZ" nil t))

(defun r4-local-uuid ()
  "Return a random (version 4) UUID."
  (format "%08x-%04x-4%03x-%04x-%012x"
          (random (expt 16 8)) (random (expt 16 4)) (random (expt 16 3))
          (logior #x8000 (random #x4000)) (random (expt 16 12))))

(defun r4-local-slugify (name)
  "Return a channel slug made from NAME."
  (string-trim (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase name)) "-+" "-+"))

(defun r4-local-media-id (url)
  "Return the YouTube video id of URL, or nil."
  (when (string-match (rx (or "youtube.com/watch?v=" "youtu.be/" "youtube.com/shorts/")
                          (group (+ (any alnum "_-"))))
                      url)
    (match-string 1 url)))

(defun r4-local-tags (description)
  "Return the #tags of DESCRIPTION, without the hash, like radio4000.com."
  (let ((tags nil) (start 0))
    (while (and description (string-match "#\\([[:alnum:]_-]+\\)" description start))
      (cl-pushnew (downcase (match-string 1 description)) tags :test #'equal)
      (setq start (match-end 0)))
    (nreverse tags)))

;;;; JSON

(defun r4-local--json-value (value)
  "Return VALUE ready for `json-serialize'."
  (cond ((null value) :null)
        ((consp value) (vconcat value))
        (t value)))

(defun r4-local--channel-to-json (channel)
  "Return CHANNEL as an alist for `json-serialize'."
  (cl-loop for (key . accessor) in '((id . r4-channel-id) (slug . r4-channel-slug)
                                     (name . r4-channel-name)
                                     (description . r4-channel-description)
                                     (url . r4-channel-url) (image . r4-channel-image)
                                     (track_count . r4-channel-track-count)
                                     (latest_track_at . r4-channel-latest-track-at)
                                     (created_at . r4-channel-created-at)
                                     (updated_at . r4-channel-updated-at)
                                     (latitude . r4-channel-latitude)
                                     (longitude . r4-channel-longitude))
           collect (cons key (r4-local--json-value (funcall accessor channel)))))

(defun r4-local--track-to-json (track)
  "Return TRACK as an alist for `json-serialize'."
  (cl-loop for (key . accessor) in '((id . r4-track-id) (slug . r4-track-slug)
                                     (title . r4-track-title) (url . r4-track-url)
                                     (description . r4-track-description)
                                     (tags . r4-track-tags) (duration . r4-track-duration)
                                     (provider . r4-track-provider)
                                     (media_id . r4-track-media-id)
                                     (discogs_url . r4-track-discogs-url)
                                     (created_at . r4-track-created-at)
                                     (updated_at . r4-track-updated-at))
           collect (cons key (if (eq key 'tags)
                                 (vconcat (funcall accessor track))
                               (r4-local--json-value (funcall accessor track))))))

(defun r4-local-read (slug)
  "Return the local channel SLUG as a cons of an `r4-channel' and its tracks."
  (let ((data (with-temp-buffer
                (insert-file-contents (r4-local-file slug))
                (json-parse-buffer :object-type 'alist :array-type 'list
                                   :null-object nil :false-object nil))))
    (cons (r4-channel-from-json (alist-get 'channel data))
          (mapcar #'r4-track-from-json (alist-get 'tracks data)))))

(defun r4-local-write (channel tracks)
  "Write the local CHANNEL with its TRACKS, newest first."
  (let ((slug (r4-channel-slug channel)))
    (setf (r4-channel-track-count channel) (length tracks)
          (r4-channel-latest-track-at channel) (and tracks (r4-track-created-at (car tracks)))
          (r4-channel-updated-at channel) (r4-local-now))
    (make-directory (r4-files-channel-directory slug) t)
    (with-temp-file (r4-local-file slug)
      (setq buffer-file-coding-system 'utf-8-unix)
      (json-insert `((version . 2)
                     (created_at . ,(r4-local-now))
                     (channel . ,(r4-local--channel-to-json channel))
                     (tracks . ,(vconcat (mapcar #'r4-local--track-to-json tracks)))))
      (json-pretty-print-buffer))
    (r4-files-write-playlist slug tracks)))

(defun r4-local--fetch (source callback)
  "Call CALLBACK with the tracks of SOURCE if it is a local channel.
Return non-nil if SOURCE is local."
  (when (plist-get source :local)
    (let* ((tracks (cdr (r4-local-read (plist-get source :slug))))
           (query (plist-get source :query)))
      (funcall callback
               (if query
                   (cl-remove-if-not
                    (lambda (track)
                      (string-match-p (regexp-quote query)
                                      (concat (r4-track-title track) " "
                                              (r4-track-description track))))
                    tracks)
                 tracks)))
    t))

(add-hook 'r4-tracks-fetch-functions #'r4-local--fetch)

;;;; Commands

(defvar r4-local-channel-history nil
  "Minibuffer history of local channel slugs.")

(defun r4-read-local-channel (prompt &optional allow-new)
  "Read the slug of a local channel with PROMPT.
If ALLOW-NEW is non-nil, a new name creates the channel."
  (let* ((slugs (r4-local-slugs))
         (slug (completing-read prompt slugs nil (not allow-new) nil
                                'r4-local-channel-history)))
    (cond ((member slug slugs) slug)
          ((string-blank-p slug) (user-error "No channel"))
          ((y-or-n-p (format "Create the local channel \"%s\"? " slug))
           (r4-channel-slug (r4-local-create-channel slug)))
          (t (user-error "Cancelled")))))

;;;###autoload
(defun r4-local-create-channel (name &optional description)
  "Create a local channel named NAME, with an optional DESCRIPTION.
Return the new `r4-channel'."
  (interactive (list (read-string "Channel name: ")
                     (read-string "Description: ")))
  (let ((slug (r4-local-slugify name))
        (now (r4-local-now)))
    (when (string-empty-p slug) (user-error "Invalid channel name"))
    (when (r4-local-p slug) (user-error "Local channel @%s already exists" slug))
    (let ((channel (r4-channel-create :id (r4-local-uuid) :slug slug :name name
                                      :description description
                                      :created-at now :updated-at now)))
      (r4-local-write channel nil)
      (message "Created the local channel @%s in %s" slug (r4-local-file slug))
      channel)))

;;;###autoload
(defun r4-local-open-channel (slug)
  "Show the tracks of the local channel SLUG."
  (interactive (list (r4-read-local-channel "Local channel: @")))
  (r4-tracks-show (list :slug slug :local t)))

(defun r4-local--add (slug tracks)
  "Add copies of TRACKS to the local channel SLUG, and return them."
  (let* ((local (r4-local-read slug))
         (now (r4-local-now))
         (copies (mapcar (lambda (track)
                           (r4-track-create
                            :id (r4-local-uuid) :slug slug
                            :title (r4-track-title track) :url (r4-track-url track)
                            :description (r4-track-description track)
                            :tags (r4-track-tags track)
                            :duration (r4-track-duration track)
                            :provider (r4-track-provider track)
                            :media-id (r4-track-media-id track)
                            :discogs-url (r4-track-discogs-url track)
                            :created-at now :updated-at now))
                         tracks)))
    (r4-local-write (car local) (append copies (cdr local)))
    (r4-local--refresh slug)
    copies))

(defun r4-local--refresh (slug)
  "Fetch again the buffers showing the local channel SLUG."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'r4-tracks-mode)
                 (plist-get r4-tracks--source :local)
                 (equal (plist-get r4-tracks--source :slug) slug))
        (revert-buffer)))))

;;;###autoload
(defun r4-local-add-track (slug url title &optional description)
  "Add the track URL with TITLE and DESCRIPTION to the local channel SLUG.
Interactively, SLUG is the local channel of the list, or else asked,
and the title is fetched from the URL, if possible."
  (interactive
   (let ((slug (or (and (derived-mode-p 'r4-tracks-mode)
                        (plist-get r4-tracks--source :local)
                        (plist-get r4-tracks--source :slug))
                   (r4-read-local-channel "Add track to local channel: @" t)))
         (fields (r4-tracks-read-fields)))
     (list slug (plist-get fields :url) (plist-get fields :title)
           (plist-get fields :description))))
  (when (string-blank-p url) (user-error "Missing URL"))
  (let ((media-id (r4-local-media-id url)))
    (r4-local--add slug (list (r4-track-create
                               :url url :title title :description description
                               :tags (r4-local-tags description)
                               :provider (and media-id "youtube") :media-id media-id))))
  (message "Added \"%s\" to @%s" title slug))

(defun r4-tracks-copy-to-local (slug)
  "Add the marked tracks, or the track at point, to the local channel SLUG."
  (interactive (list (r4-read-local-channel "Add to local channel: @" t)))
  (let ((tracks (r4-tracks--marked)))
    (r4-local--add slug tracks)
    (tabulated-list-clear-all-tags)
    (message "Added %d track%s to @%s" (length tracks) (if (cdr tracks) "s" "") slug)))

(defun r4-tracks-remove-from-local ()
  "Remove the marked tracks, or the track at point, from this local channel."
  (interactive)
  (let ((slug (plist-get r4-tracks--source :slug)))
    (unless (plist-get r4-tracks--source :local)
      (user-error "Not a local channel"))
    (let* ((ids (mapcar #'r4-track-id (r4-tracks--marked)))
           (local (r4-local-read slug)))
      (when (yes-or-no-p (format "Remove %d track%s from @%s? "
                                 (length ids) (if (cdr ids) "s" "") slug))
        (r4-local-write (car local)
                        (cl-remove-if (lambda (track) (member (r4-track-id track) ids))
                                      (cdr local)))
        (r4-local--refresh slug)))))

;;;###autoload
(defun r4-local-save-channel (slug)
  "Save a local copy of the remote channel SLUG, to browse it offline.
An existing local channel SLUG is replaced."
  (interactive (list (r4-read-channel "Save channel locally: @")))
  (when (or (not (r4-local-p slug))
            (yes-or-no-p (format "Replace the local channel @%s? " slug)))
    (message "Fetching @%s…" slug)
    (r4-api-channel
     slug
     (lambda (channel)
       (unless channel (user-error "No channel @%s" slug))
       (r4-api-tracks (lambda (tracks)
                        (r4-local-write channel tracks)
                        (message "Saved @%s (%d tracks) to %s"
                                 slug (length tracks) (r4-local-file slug)))
                      :slug slug)))))

(defun r4-tracks-save-local ()
  "Save a local copy of the channel of this buffer."
  (interactive)
  (r4-local-save-channel (or (plist-get r4-tracks--source :slug)
                             (user-error "Not a channel"))))

(keymap-set r4-tracks-mode-map "c" #'r4-tracks-copy-to-local)
(keymap-set r4-tracks-mode-map "x" #'r4-tracks-remove-from-local)
(keymap-set r4-tracks-mode-map "C" #'r4-tracks-save-local)

(defun r4-channels-save-local ()
  "Save a local copy of the channel at point."
  (interactive)
  (r4-local-save-channel (r4-channel-slug (r4-channels--at-point))))

(keymap-set r4-channels-mode-map "C" #'r4-channels-save-local)

(provide 'r4-local)

;;; r4-local.el ends here
