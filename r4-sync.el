;;; r4-sync.el --- Sync local channels with radio4000.com  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Sync a local channel with its radio4000.com channel, for a signed
;; in user.  A local channel that does not exist yet is published.
;; Tracks only on one side are copied to the other side, so a sync
;; never deletes anything: remove tracks on both sides to delete them.
;;
;; Local and remote channels and tracks share the same ids, created
;; locally, so syncing again is safe.

;;; Code:

(require 'cl-lib)
(require 'r4-models)
(require 'r4-api)
(require 'r4-auth)
(require 'r4-local)

(defcustom r4-sync-batch-size 500
  "Maximum number of tracks sent in one request."
  :type 'natnum
  :group 'r4)

(defun r4-sync--rest (method table &rest args)
  "Send a METHOD request to the REST TABLE as the signed in user.
ARGS are passed to `r4-api-request'."
  (apply #'r4-api-request method (concat "/rest/v1/" table) :token (r4-auth-token) args))

(defun r4-sync--remote-channel (column value)
  "Return the remote channel whose COLUMN is VALUE, as an `r4-channel', or nil."
  (when-let* ((row (car (r4-sync--rest "GET" "channels"
                                       :params `(("select" "id,slug,name,description,url,created_at,updated_at")
                                                 (,column ,(concat "eq." value)))))))
    (r4-channel-from-json row)))

(defun r4-sync--owned-p (channel user-id)
  "Return non-nil if the remote CHANNEL belongs to USER-ID."
  (r4-sync--rest "GET" "user_channel"
                 :params `(("select" "channel_id")
                           ("channel_id" ,(concat "eq." (r4-channel-id channel)))
                           ("user_id" ,(concat "eq." user-id)))))

(defun r4-sync-my-channels ()
  "Return the remote channels of the signed in user, as `r4-channel'."
  (mapcar #'r4-channel-from-json
          (r4-sync--rest "GET" "channels"
                         :params `(("select" "id,slug,name,description,user_channel!inner(user_id)")
                                   ("user_channel.user_id" ,(concat "eq." (r4-auth-user-id)))
                                   ("order" "updated_at.desc")))))

(defun r4-sync--create-channel (channel user-id)
  "Create the remote CHANNEL, owned by USER-ID."
  (r4-sync--rest "POST" "channels"
                 :prefer "return=minimal"
                 :body `((id . ,(r4-channel-id channel))
                         (name . ,(r4-channel-name channel))
                         (slug . ,(r4-channel-slug channel))
                         (description . ,(or (r4-channel-description channel) ""))
                         (url . ,(or (r4-channel-url channel) ""))))
  (r4-sync--rest "POST" "user_channel"
                 :prefer "return=minimal"
                 :body `((user_id . ,user-id) (channel_id . ,(r4-channel-id channel)))))

(defun r4-sync--remote-tracks (slug)
  "Return the tracks of the remote channel SLUG."
  (mapcar #'r4-track-from-json
          (r4-sync--rest "GET" "channel_tracks"
                         :params `(("select" ,r4-api--track-columns)
                                   ("slug" ,(concat "eq." slug))
                                   ("order" "created_at.desc")))))

(defun r4-sync--push-tracks (tracks channel-id user-id)
  "Create the remote TRACKS in the channel CHANNEL-ID, owned by USER-ID."
  (let ((now (r4-local-now)))
    (cl-loop
     for batch in (seq-split tracks r4-sync-batch-size)
     do (r4-sync--rest
         "POST" "tracks"
         :prefer "return=minimal"
         :body (vconcat
                (mapcar (lambda (track)
                          `((id . ,(r4-track-id track))
                            (url . ,(r4-track-url track))
                            (title . ,(r4-track-title track))
                            (description . ,(or (r4-track-description track) ""))
                            (discogs_url . ,(or (r4-track-discogs-url track) :null))
                            (created_at . ,(or (r4-track-created-at track) now))))
                        batch)))
     (r4-sync--rest
      "POST" "channel_track"
      :prefer "return=minimal"
      :body (vconcat
             (mapcar (lambda (track)
                       `((track_id . ,(r4-track-id track))
                         (channel_id . ,channel-id)
                         (user_id . ,user-id)
                         (created_at . ,(or (r4-track-created-at track) now))))
                     batch))))))

(defun r4-sync--resolve-remote (channel user-id)
  "Return the remote channel of the local CHANNEL for USER-ID, or nil.
Signal an error if CHANNEL cannot be synced.  When the local channel
was created offline and USER-ID already has a remote channel with the
same slug, offer to link them."
  (let ((slug (r4-channel-slug channel)))
    (if-let* ((remote (r4-sync--remote-channel "id" (r4-channel-id channel))))
        (if (r4-sync--owned-p remote user-id)
            remote
          (user-error "@%s belongs to another account, this local channel is a saved copy" slug))
      (when-let* ((taken (r4-sync--remote-channel "slug" slug)))
        (unless (r4-sync--owned-p taken user-id)
          (user-error "The slug @%s is already used on radio4000.com" slug))
        (unless (yes-or-no-p (format "Link this local channel to your radio4000.com channel @%s? "
                                     slug))
          (user-error "Cancelled"))
        taken))))

;;;###autoload
(defun r4-sync-channel (slug)
  "Sync the local channel SLUG with radio4000.com.
Publish the channel if needed, upload the tracks only in the local
channel and download the tracks only in the remote channel."
  (interactive (list (r4-read-local-channel "Sync local channel: @")))
  (let* ((user-id (r4-auth-user-id))
         (local (r4-local-read slug))
         (channel (car local))
         (tracks (cdr local))
         (remote (r4-sync--resolve-remote channel user-id)))
    (when (and remote (not (equal (r4-channel-id remote) (r4-channel-id channel))))
      (setf (r4-channel-id channel) (r4-channel-id remote)))
    (let* ((remote-tracks (and remote (r4-sync--remote-tracks (r4-channel-slug remote))))
           (remote-ids (mapcar #'r4-track-id remote-tracks))
           (local-ids (mapcar #'r4-track-id tracks))
           (to-push (cl-remove-if (lambda (track) (member (r4-track-id track) remote-ids)) tracks))
           (to-pull (cl-remove-if (lambda (track) (member (r4-track-id track) local-ids))
                                  remote-tracks)))
      (if (and remote (null to-push) (null to-pull))
          (message "@%s is in sync with radio4000.com" slug)
        (unless (yes-or-no-p
                 (format "%s@%s: upload %d track%s, download %d track%s? "
                         (if remote "Sync " "Publish ") slug
                         (length to-push) (if (= (length to-push) 1) "" "s")
                         (length to-pull) (if (= (length to-pull) 1) "" "s")))
          (user-error "Cancelled"))
        (unless remote
          (message "Publishing @%s…" slug)
          (r4-sync--create-channel channel user-id))
        (when to-push
          (message "Uploading %d tracks…" (length to-push))
          (r4-sync--push-tracks to-push (r4-channel-id channel) user-id))
        (r4-local-write
         channel
         (sort (append tracks
                       (mapcar (lambda (track) (setf (r4-track-slug track) slug) track)
                               to-pull))
               (lambda (a b) (string> (or (r4-track-created-at a) "")
                                      (or (r4-track-created-at b) "")))))
        (r4-local--refresh slug)
        (message "Synced @%s: uploaded %d, downloaded %d tracks — %s"
                 slug (length to-push) (length to-pull) (r4-channel-web-url channel))))))

;;;###autoload
(defun r4-sync-pull-channel (slug)
  "Save a local copy of your radio4000.com channel SLUG, to sync it later."
  (interactive
   (list (completing-read "Your channel: @"
                          (mapcar #'r4-channel-slug (r4-sync-my-channels)) nil t)))
  (r4-local-save-channel slug))

(defun r4-tracks-sync ()
  "Sync the local channel of this buffer with radio4000.com."
  (interactive)
  (unless (plist-get r4-tracks--source :local)
    (user-error "Not a local channel"))
  (r4-sync-channel (plist-get r4-tracks--source :slug)))

(keymap-set r4-tracks-mode-map "Y" #'r4-tracks-sync)

(provide 'r4-sync)

;;; r4-sync.el ends here
