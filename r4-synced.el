;;; r4-synced.el --- Keep Radio4000 channels synced locally  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Choose channels to keep synced locally, such as a few of the channels
;; you follow: their new tracks download into `r4-download-directory',
;; when you run `r4-sync-channels', and when you open an r4 list, at
;; most every `r4-sync-interval' seconds.  Tracks of synced channels
;; also download when they play, see `r4-player-download'.
;;
;; y toggles syncing the channel at point in channel lists, or the
;; channel of a track list.

;;; Code:

(require 'cl-lib)
(require 'r4-models)
(require 'r4-api)
(require 'r4-files)
(require 'r4-download)
(require 'r4-player)
(require 'r4-tracks)
(require 'r4-channels)

(defcustom r4-synced-channels nil
  "Slugs of the channels kept synced: their new tracks download.
Toggle a channel with `r4-toggle-synced-channel', y in lists."
  :type '(repeat string)
  :group 'r4)

(defcustom r4-sync-interval 3600
  "Seconds between two checks of the synced channels, when opening a list.
nil checks only with `r4-sync-channels'."
  :type '(choice (natnum :tag "Seconds") (const :tag "Only on demand" nil))
  :group 'r4)

(defcustom r4-player-download 'synced
  "Which tracks to download while they play, to not stream them again.
`synced' downloads the tracks of `r4-synced-channels', t all tracks,
and nil none."
  :type '(choice (const :tag "Tracks of synced channels" synced)
                 (const :tag "All tracks" t)
                 (const :tag "None" nil))
  :group 'r4)

(defvar r4-synced-save-function #'customize-save-variable
  "Function saving `r4-synced-channels', called with the symbol and value.")

(defvar r4-synced--last-check nil
  "Time of the last check of the synced channels.")

(defun r4-synced-p (slug)
  "Return non-nil if the channel SLUG is synced."
  (member slug r4-synced-channels))

(setq r4-channels-synced-function #'r4-synced-p)

;;;###autoload
(defun r4-toggle-synced-channel (slug)
  "Start or stop syncing the channel SLUG locally."
  (interactive (list (r4-read-channel "Sync or unsync channel: @")))
  (let ((synced (not (r4-synced-p slug))))
    (funcall r4-synced-save-function 'r4-synced-channels
             (if synced
                 (append r4-synced-channels (list slug))
               (delete slug (copy-sequence r4-synced-channels))))
    (message (if synced
                 "@%s is synced: its new tracks download, see r4-sync-channels"
               "@%s is not synced anymore")
             slug)
    (r4-channels-status-changed)
    (when synced (r4-sync-channels (list slug)))))

(defun r4-channels-toggle-sync ()
  "Start or stop syncing the channel at point locally."
  (interactive)
  (r4-toggle-synced-channel (r4-channel-slug (r4-channels--at-point))))

(defun r4-tracks-toggle-sync ()
  "Start or stop syncing the channel of this list locally."
  (interactive)
  (let ((slug (plist-get r4-tracks--source :slug)))
    (when (or (null slug) (plist-get r4-tracks--source :local)
              (plist-get r4-tracks--source :query))
      (user-error "Not the list of a radio4000.com channel"))
    (r4-toggle-synced-channel slug)))

;;;###autoload
(defun r4-sync-channels (&optional slugs)
  "Download the new tracks of the synced channels, or of SLUGS.
Tracks already downloaded, or whose download failed before, are
skipped."
  (interactive)
  (let ((slugs (or slugs r4-synced-channels)))
    (unless slugs
      (user-error "No synced channel, sync one with y in a channel list"))
    (setq r4-synced--last-check (float-time))
    (dolist (slug slugs)
      (r4-api-channel
       slug
       (lambda (channel)
         (when channel
           (r4-api-tracks (lambda (tracks)
                            (r4-download-tracks tracks :channel channel
                                                :channel-tracks tracks))
                          :slug slug)))))))

(defun r4-sync-channels-maybe ()
  "Check the synced channels, if not done for `r4-sync-interval' seconds."
  (when (and r4-synced-channels r4-sync-interval
             (executable-find r4-download-program)
             (or (null r4-synced--last-check)
                 (> (- (float-time) r4-synced--last-check) r4-sync-interval)))
    (let ((r4-api-error-function #'ignore))
      (r4-sync-channels))))

(add-hook 'r4-channels-mode-hook #'r4-sync-channels-maybe)
(add-hook 'r4-tracks-mode-hook #'r4-sync-channels-maybe)

;;;###autoload
(defun r4-list-synced-channels ()
  "List the synced channels, y stops syncing the channel at point."
  (interactive)
  (unless r4-synced-channels
    (user-error "No synced channel, sync one with y in a channel list"))
  (r4-channels-show "synced channels"
                    (lambda (callback)
                      (r4-api-get "channels_with_tracks"
                                  `(("select" ,r4-api--channel-columns)
                                    ("slug" ,(format "in.(%s)"
                                                     (string-join r4-synced-channels ","))))
                                  (lambda (rows)
                                    (funcall callback (mapcar #'r4-channel-from-json rows)))))
                    (list 'r4-list-synced-channels)))

;;;; Download while playing

(defun r4-synced--download-playing ()
  "Download the playing track, according to `r4-player-download'."
  (when-let* ((track (and r4-player-download (r4-player-live-p) (r4-player-current))))
    (when (and (or (eq r4-player-download t) (r4-synced-p (r4-track-slug track)))
               (not (r4-files-track-file track))
               (not (r4-download-pending-p track))
               (executable-find r4-download-program))
      (let ((inhibit-message t))
        (r4-download-tracks (list track))))))

(add-hook 'r4-player-track-changed-hook #'r4-synced--download-playing)

(keymap-set r4-channels-mode-map "y" #'r4-channels-toggle-sync)
(keymap-set r4-tracks-mode-map "y" #'r4-tracks-toggle-sync)

(provide 'r4-synced)

;;; r4-synced.el ends here
