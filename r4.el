;;; r4.el --- Browse, play and download Radio4000 channels  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; Author: H. Ugrp <h.u.g.u.r.p+emacs@pm.me>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: multimedia, hypermedia
;; URL: https://github.com/radio4000/r4.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A Radio4000 (https://radio4000.com) client for Emacs.
;;
;; - `r4' opens a menu with all the commands.
;; - `r4-channels' lists the channels, `r4-open-channel' lists the
;;   tracks of a channel, `r4-search-tracks' searches all tracks.
;; - Tracks are played with mpv, from disk when downloaded.
;; - Tracks are downloaded as high quality audio with yt-dlp into
;;   `r4-download-directory' (~/Music/radio4000/ by default).
;; - Local channels live in files, without a radio4000.com account,
;;   and sync with radio4000.com after `r4-login'.
;;
;; Requires the programs mpv, yt-dlp and ffmpeg.

;;; Code:

(require 'transient)
(require 'r4-models)
(require 'r4-api)
(require 'r4-download)
(require 'r4-player)
(require 'r4-tracks)
(require 'r4-channels)
(require 'r4-local)
(require 'r4-auth)
(require 'r4-sync)

;;;###autoload (autoload 'r4 "r4" nil t)
(transient-define-prefix r4 ()
  "Radio4000 menu."
  [["Browse"
    ("c" "Channels" r4-channels)
    ("C" "Search channels" r4-search-channels)
    ("o" "Open channel" r4-open-channel)
    ("i" "Describe channel" r4-describe-channel)
    ("t" "Search tracks" r4-search-tracks)]
   ["Player"
    ("P" "Play channel" r4-play-channel)
    ("SPC" "Pause/resume" r4-player-toggle-pause :transient t)
    ("n" "Next" r4-player-next :transient t)
    ("p" "Previous" r4-player-previous :transient t)
    ("f" "Seek forward" r4-player-seek-forward :transient t)
    ("b" "Seek backward" r4-player-seek-backward :transient t)
    ("+" "Volume up" r4-player-volume-up :transient t)
    ("-" "Volume down" r4-player-volume-down :transient t)
    ("z" "Toggle shuffle" r4-player-toggle-shuffle :transient t)
    ("Q" "Queue" r4-queue)
    ("w" "Copy current track media URL" r4-player-copy-url)
    ("W" "Copy current track radio4000.com URL" r4-player-copy-web-url)
    ("e" "Open current track on radio4000.com" r4-player-browse-current)
    ("k" "Stop" r4-player-stop)]
   ["Download"
    ("D" "Download channel" r4-download-channel)
    ("L" "Downloads" r4-downloads)
    ("F" "Open music folder" r4-download-open-directory)
    ("K" "Cancel all downloads" r4-download-cancel-all)]
   ["Local"
    ("l" "Open local channel" r4-local-open-channel)
    ("N" "New local channel" r4-local-create-channel)
    ("a" "Add track by URL" r4-local-add-track)
    ("S" "Save channel locally" r4-local-save-channel)]
   ["Account"
    ("Y" "Sync local channel" r4-sync-channel)
    ("G" "Get one of my channels" r4-sync-pull-channel)
    ("I" "Sign in" r4-login)
    ("O" "Sign out" r4-logout)
    ("M" "Who am I" r4-whoami)]])

(define-obsolete-function-alias 'r4-status #'r4-channels "0.2.0")
(define-obsolete-function-alias 'list-r4-channels #'r4-channels "0.2.0")
(define-obsolete-function-alias 'r4-describe-channel-slug #'r4-open-channel "0.2.0")
(define-obsolete-function-alias 'r4-kill-player #'r4-player-stop "0.2.0")
(define-obsolete-function-alias 'r4-player-play-next-track #'r4-player-next "0.2.0")

(provide 'r4)

;;; r4.el ends here
