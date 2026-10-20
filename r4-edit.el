;;; r4-edit.el --- Edit Radio4000 channels and tracks  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Create, edit and delete your channels and their tracks, on
;; radio4000.com or in local channels, with the same keys: A adds, e
;; edits and x deletes, in track and channel lists.

;;; Code:

(require 'cl-lib)
(require 'r4-models)
(require 'r4-api)
(require 'r4-auth)
(require 'r4-sync)
(require 'r4-local)
(require 'r4-tracks)
(require 'r4-channels)

;;;; Your channels

(defvar r4-edit--my-channels nil
  "Cache of your remote channels, as (USER-ID . CHANNELS).")

(defun r4-edit-my-channels ()
  "Return your remote channels, signing in first if needed."
  (let ((user-id (r4-auth-user-id)))
    (unless (equal (car r4-edit--my-channels) user-id)
      (setq r4-edit--my-channels (cons user-id (r4-sync-my-channels))))
    (cdr r4-edit--my-channels)))

(defun r4-edit-my-channel (slug)
  "Return your remote channel SLUG, or signal an error."
  (or (cl-find slug (r4-edit-my-channels) :key #'r4-channel-slug :test #'equal)
      (user-error "@%s is not one of your channels" slug)))

(defun r4-edit--read-my-channel (prompt)
  "Read the slug of one of your channels with PROMPT."
  (let ((slugs (mapcar #'r4-channel-slug (r4-edit-my-channels))))
    (if (cdr slugs)
        (completing-read prompt slugs nil t)
      (or (car slugs) (user-error "Your account has no channel")))))

;;;; Tracks

;;;###autoload
(defun r4-add-track (slug)
  "Add a track to your radio4000.com channel SLUG."
  (interactive (list (r4-edit--read-my-channel "Add a track to your channel: @")))
  (let* ((channel (r4-edit-my-channel slug))
         (fields (r4-tracks-read-fields))
         (track (r4-track-create :id (r4-local-uuid) :slug slug
                                 :url (plist-get fields :url)
                                 :title (plist-get fields :title)
                                 :description (plist-get fields :description)
                                 :created-at (r4-local-now))))
    (r4-sync--push-tracks (list track) (r4-channel-id channel) (r4-auth-user-id))
    (message "Added \"%s\" to @%s" (r4-track-title track) slug)
    (r4-edit--refresh slug)))

(defun r4-edit--refresh (slug)
  "Fetch again the track lists of the remote channel SLUG."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'r4-tracks-mode)
                 (not (plist-get r4-tracks--source :local))
                 (equal (plist-get r4-tracks--source :slug) slug))
        (revert-buffer)))))

(defun r4-edit--remote-slug ()
  "Return the slug of the remote channel of the track list, if yours."
  (let ((slug (plist-get r4-tracks--source :slug)))
    (unless (and slug (not (plist-get r4-tracks--source :query)))
      (user-error "Not a channel"))
    (r4-channel-slug (r4-edit-my-channel slug))))

(defun r4-tracks-create ()
  "Add a track to the channel of this list, local or yours."
  (interactive)
  (if (plist-get r4-tracks--source :local)
      (call-interactively #'r4-local-add-track)
    (r4-add-track (r4-edit--remote-slug))))

(defun r4-tracks-edit ()
  "Edit the URL, title and description of the track at point."
  (interactive)
  (let* ((track (r4-tracks--at-point))
         (local (plist-get r4-tracks--source :local))
         (slug (if local (r4-track-slug track) (r4-edit--remote-slug)))
         (fields (r4-tracks-read-fields track)))
    (if local
        (let ((data (r4-local-read slug)))
          (dolist (each (cdr data))
            (when (equal (r4-track-id each) (r4-track-id track))
              (setf (r4-track-url each) (plist-get fields :url)
                    (r4-track-title each) (plist-get fields :title)
                    (r4-track-description each) (plist-get fields :description)
                    (r4-track-tags each) (r4-local-tags (plist-get fields :description))
                    (r4-track-updated-at each) (r4-local-now))))
          (r4-local-write (car data) (cdr data))
          (r4-local--refresh slug))
      (r4-sync--rest "PATCH" "tracks"
                     :params `(("id" ,(concat "eq." (r4-track-id track))))
                     :prefer "return=minimal"
                     :body `((url . ,(plist-get fields :url))
                             (title . ,(plist-get fields :title))
                             (description . ,(plist-get fields :description))))
      (r4-edit--refresh slug))
    (message "Saved \"%s\"" (plist-get fields :title))))

(defun r4-tracks-delete ()
  "Delete the marked tracks, or the track at point, from this channel.
In a local channel, remove them from the local channel."
  (interactive)
  (if (plist-get r4-tracks--source :local)
      (r4-tracks-remove-from-local)
    (let ((slug (r4-edit--remote-slug))
          (tracks (r4-tracks--marked)))
      (when (yes-or-no-p (format "Delete %d track%s from @%s on radio4000.com? "
                                 (length tracks) (if (cdr tracks) "s" "") slug))
        (r4-sync--rest "DELETE" "tracks"
                       :params `(("id" ,(format "in.(%s)"
                                                (mapconcat #'r4-track-id tracks ",")))))
        (message "Deleted %d track%s" (length tracks) (if (cdr tracks) "s" ""))
        (r4-edit--refresh slug)))))

;;;; Channels

;;;###autoload
(defun r4-create-channel (name slug &optional description)
  "Create the radio4000.com channel NAME, at SLUG, with DESCRIPTION."
  (interactive
   (let ((name (read-string "Channel name: ")))
     (list name
           (read-string "Slug: @" (r4-local-slugify name))
           (read-string "Description: "))))
  (let ((user-id (r4-auth-user-id))
        (channel (r4-channel-create :id (r4-local-uuid) :name name :slug slug
                                    :description description)))
    (when (r4-sync--remote-channel "slug" slug)
      (user-error "The slug @%s is already used" slug))
    (r4-sync--create-channel channel user-id)
    (setq r4-edit--my-channels nil)
    (message "Created @%s: %s" slug (r4-channel-web-url channel))
    channel))

(defun r4-edit-channel (slug)
  "Edit the name, slug, description and link of your channel SLUG."
  (interactive (list (r4-edit--read-my-channel "Edit your channel: @")))
  (let* ((channel (r4-edit-my-channel slug))
         (name (read-string "Name: " (r4-channel-name channel)))
         (new-slug (read-string "Slug: @" (r4-channel-slug channel)))
         (description (read-string "Description: " (r4-channel-description channel)))
         (url (read-string "Link: " (r4-channel-url channel))))
    (r4-sync--rest "PATCH" "channels"
                   :params `(("id" ,(concat "eq." (r4-channel-id channel))))
                   :prefer "return=minimal"
                   :body `((name . ,name) (slug . ,new-slug)
                           (description . ,description) (url . ,url)))
    (setq r4-edit--my-channels nil)
    (message "Saved @%s" new-slug)))

(defun r4-delete-channel (slug)
  "Delete your channel SLUG and its tracks from radio4000.com."
  (interactive (list (r4-edit--read-my-channel "Delete your channel: @")))
  (let ((channel (r4-edit-my-channel slug)))
    (unless (equal (read-string (format "Type %s to delete @%s and all its tracks: " slug slug))
                   slug)
      (user-error "Cancelled"))
    (r4-sync--rest "DELETE" "channels"
                   :params `(("id" ,(concat "eq." (r4-channel-id channel)))))
    (setq r4-edit--my-channels nil)
    (message "Deleted @%s" slug)))

(defun r4-channels-edit ()
  "Edit the channel at point, if yours."
  (interactive)
  (r4-edit-channel (r4-channel-slug (r4-channels--at-point))))

(defun r4-channels-delete ()
  "Delete the channel at point, if yours."
  (interactive)
  (r4-delete-channel (r4-channel-slug (r4-channels--at-point))))

;;;###autoload
(defun r4-my-channels ()
  "List your radio4000.com channels."
  (interactive)
  (setq r4-edit--my-channels nil)
  (let ((ids (mapcar #'r4-channel-id (r4-edit-my-channels))))
    (r4-channels-show "my channels"
                      (lambda (callback) (r4-api-channels-by-id ids callback))
                      (list 'r4-my-channels))))

(keymap-set r4-tracks-mode-map "A" #'r4-tracks-create)
(keymap-set r4-tracks-mode-map "e" #'r4-tracks-edit)
(keymap-set r4-tracks-mode-map "x" #'r4-tracks-delete)
(keymap-set r4-channels-mode-map "A" #'r4-create-channel)
(keymap-set r4-channels-mode-map "e" #'r4-channels-edit)
(keymap-set r4-channels-mode-map "x" #'r4-channels-delete)

(provide 'r4-edit)

;;; r4-edit.el ends here
