;;; r4-follows.el --- Follow Radio4000 channels  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A channel follows other channels, like the follow button of
;; radio4000.com.  Your channel follows with `r4-channels-follow' (f in
;; channel lists), `r4-following' and `r4-followers' list the channels
;; a channel follows and its followers, and `r4-feed' lists the latest
;; tracks of the channels your channel follows.

;;; Code:

(require 'cl-lib)
(require 'r4-models)
(require 'r4-api)
(require 'r4-auth)
(require 'r4-sync)
(require 'r4-channels)
(require 'r4-tracks)

(defcustom r4-my-channel nil
  "Slug of your channel, used to follow channels and for your feed.
When nil and your account has several channels, you choose one once
per Emacs session."
  :type '(choice (const :tag "Ask" nil) (string :tag "Slug"))
  :group 'r4)

(defcustom r4-feed-size 300
  "Maximum number of tracks in the feed."
  :type 'natnum
  :group 'r4)

(defvar r4-follows--my-channel nil
  "Your `r4-channel', as chosen in this session.")

(defvar r4-follows--following-state nil
  "Nil until the followed channels are fetched, then `done'.")

(defun r4-follows-my-channel ()
  "Return your `r4-channel', signing in first if needed."
  (or (and r4-follows--my-channel
           (or (null r4-my-channel)
               (equal r4-my-channel (r4-channel-slug r4-follows--my-channel)))
           r4-follows--my-channel)
      (let* ((channels (or (r4-sync-my-channels)
                           (user-error "Your account has no channel")))
             (slug (or r4-my-channel
                       (if (cdr channels)
                           (completing-read "Your channel: @"
                                            (mapcar #'r4-channel-slug channels) nil t)
                         (r4-channel-slug (car channels))))))
        (setq r4-follows--following-state nil
              r4-follows--my-channel
              (or (cl-find slug channels :key #'r4-channel-slug :test #'equal)
                  (user-error "@%s is not one of your channels" slug))))))

;;;; Followed channels, in channel lists

(defvar r4-follows--following nil
  "Hash table of the ids of the channels your channel follows, once fetched.")

(defun r4-follows-following-p (channel)
  "Return non-nil if your channel follows CHANNEL, as far as known."
  (and (hash-table-p r4-follows--following)
       (gethash (r4-channel-id channel) r4-follows--following)))

(defun r4-follows--quiet-my-channel ()
  "Return your channel without asking which one, or nil."
  (or r4-follows--my-channel
      (and (r4-auth-signed-in-p)
           (condition-case nil
               (let ((channels (r4-sync-my-channels)))
                 (when (and channels (or r4-my-channel (null (cdr channels))))
                   (r4-follows-my-channel)))
             (error nil)))))

(defun r4-follows-load-following ()
  "Fetch once the channels your channel follows, to mark them in lists."
  (unless r4-follows--following-state
    (setq r4-follows--following-state 'done)
    (run-at-time
     0 nil
     (lambda ()
       (when-let* ((mine (r4-follows--quiet-my-channel)))
         (let ((r4-api-error-function #'ignore))
           (r4-api-get "followers"
                       `(("select" "channel_id")
                         ("follower_id" ,(concat "eq." (r4-channel-id mine))))
                       (lambda (rows)
                         (let ((table (make-hash-table :test #'equal)))
                           (dolist (row rows)
                             (puthash (alist-get 'channel_id row) t table))
                           (setq r4-follows--following table))
                         (r4-channels-status-changed)))))))))

(setq r4-channels-following-function #'r4-follows-following-p)
(add-hook 'r4-channels-mode-hook #'r4-follows-load-following)

(defun r4-follows--set-following (channel following)
  "Remember that your channel FOLLOWING CHANNEL or not, for the lists."
  (when (hash-table-p r4-follows--following)
    (if following
        (puthash (r4-channel-id channel) t r4-follows--following)
      (remhash (r4-channel-id channel) r4-follows--following))
    (r4-channels-status-changed (list (r4-channel-slug channel)))))

;;;; Remote

(defun r4-follows--ids (column value other)
  "Return the OTHER ids of the follows whose COLUMN is VALUE."
  (mapcar (lambda (row) (alist-get (intern other) row))
          (r4-api-get-sync "followers" `(("select" ,other)
                                         (,column ,(concat "eq." value))))))

(defun r4-follows-following-ids (channel-id)
  "Return the ids of the channels the channel CHANNEL-ID follows."
  (r4-follows--ids "follower_id" channel-id "channel_id"))

(defun r4-follows-follower-ids (channel-id)
  "Return the ids of the channels following the channel CHANNEL-ID."
  (r4-follows--ids "channel_id" channel-id "follower_id"))

(defun r4-follows-follow (channel)
  "Make your channel follow CHANNEL."
  (r4-sync--rest "POST" "followers"
                 :prefer "return=minimal"
                 :body `((follower_id . ,(r4-channel-id (r4-follows-my-channel)))
                         (channel_id . ,(r4-channel-id channel)))))

(defun r4-follows-unfollow (channel)
  "Make your channel stop following CHANNEL."
  (r4-sync--rest "DELETE" "followers"
                 :params `(("follower_id" ,(concat "eq." (r4-channel-id (r4-follows-my-channel))))
                           ("channel_id" ,(concat "eq." (r4-channel-id channel))))))

;;;; Commands

(defun r4-channels-follow ()
  "Follow the channel at point with your channel, or stop following it."
  (interactive)
  (r4-follows-toggle (r4-channels--at-point)))

(defun r4-follows-toggle (channel)
  "Follow CHANNEL with your channel, or stop following it."
  (let* ((mine (r4-follows-my-channel))
         (following (member (r4-channel-id channel)
                            (r4-follows-following-ids (r4-channel-id mine)))))
    (if following
        (when (y-or-n-p (format "Stop following @%s? " (r4-channel-slug channel)))
          (r4-follows-unfollow channel)
          (r4-follows--set-following channel nil)
          (message "@%s stopped following @%s" (r4-channel-slug mine) (r4-channel-slug channel)))
      (r4-follows-follow channel)
      (r4-follows--set-following channel t)
      (message "@%s follows @%s" (r4-channel-slug mine) (r4-channel-slug channel)))))

(defun r4-follows--channel-id (slug)
  "Return the id of the channel SLUG."
  (r4-channel-id (or (car (mapcar #'r4-channel-from-json
                                  (r4-api-get-sync "channels" `(("select" "id,slug")
                                                                ("slug" ,(concat "eq." slug))))))
                     (user-error "No channel @%s" slug))))

(defun r4-follows--read-slug (prompt)
  "Read a channel slug with PROMPT, defaulting to your channel when signed in."
  (if current-prefix-arg
      (r4-read-channel prompt)
    (r4-channel-slug (r4-follows-my-channel))))

;;;###autoload
(defun r4-following (slug)
  "List the channels that the channel SLUG follows, your channel by default.
With a prefix argument, ask for the channel."
  (interactive (list (r4-follows--read-slug "Channels followed by: @")))
  (r4-channels-show (format "following @%s" slug)
                    (lambda (callback)
                      (r4-api-channels-by-id
                       (r4-follows-following-ids (r4-follows--channel-id slug))
                       callback))
                    (list 'r4-following slug)))

;;;###autoload
(defun r4-followers (slug)
  "List the channels following the channel SLUG, your channel by default.
With a prefix argument, ask for the channel."
  (interactive (list (r4-follows--read-slug "Followers of: @")))
  (r4-channels-show (format "followers of @%s" slug)
                    (lambda (callback)
                      (r4-api-channels-by-id
                       (r4-follows-follower-ids (r4-follows--channel-id slug))
                       callback))
                    (list 'r4-followers slug)))

(defun r4-follows--fetch-feed (source callback)
  "Call CALLBACK with the latest tracks of the channels SOURCE follows.
Return non-nil if SOURCE is a feed."
  (when-let* ((slug (plist-get source :feed)))
    (r4-api-channels-by-id
     (r4-follows-following-ids (r4-follows--channel-id slug))
     (lambda (channels)
       (if channels
           (r4-api-tracks callback :slugs (mapcar #'r4-channel-slug channels)
                          :limit r4-feed-size)
         (funcall callback nil))))
    t))

(add-hook 'r4-tracks-fetch-functions #'r4-follows--fetch-feed)

;;;###autoload
(defun r4-feed (slug)
  "List the latest tracks of the channels that SLUG follows.
SLUG is your channel by default; with a prefix argument, ask for it."
  (interactive (list (r4-follows--read-slug "Feed of: @")))
  (r4-tracks-show (list :feed slug :name (format "feed of @%s" slug)
                       :empty "No tracks: follow channels with f in channel lists")))

(defun r4-channels-following ()
  "List the channels that the channel at point follows."
  (interactive)
  (r4-following (r4-channel-slug (r4-channels--at-point))))

(defun r4-channels-followers ()
  "List the channels following the channel at point."
  (interactive)
  (r4-followers (r4-channel-slug (r4-channels--at-point))))

(keymap-set r4-channels-mode-map "f" #'r4-channels-follow)
(keymap-set r4-channels-mode-map "F" #'r4-channels-following)
(keymap-set r4-channels-mode-map "r" #'r4-channels-followers)

(provide 'r4-follows)

;;; r4-follows.el ends here
