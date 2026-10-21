;;; r4.el --- Browse, play and download Radio4000 channels  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; Author: H. Ugrp <h.u.g.u.r.p+emacs@pm.me>
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: multimedia, hypermedia
;; URL: https://github.com/radio4000/r4.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A Radio4000 (https://radio4000.com) client for Emacs: everything
;; radio4000.com and the r4 command line do, in Emacs lists and menus.
;;
;; - `r4' opens the home buffer, like the home page of radio4000.com.
;;   Like in magit, ? opens the menu of the buffer you are in, and the
;;   menu of all the commands, `r4-menu', in the home buffer.
;; - `r4-channel-page' visits a channel, like /@slug.
;; - `r4-channels', `r4-explore' and `r4-search' browse channels and
;;   tracks, with the search language of radio4000.com, as in
;;   "@ko002 #jazz miles".
;; - Tracks play with mpv, from disk when downloaded, with their videos
;;   in Emacs; `r4-auto-radio' plays the same live radio as
;;   radio4000.com, `r4-broadcasts' lists live broadcasts, and
;;   `r4-broadcast-toggle' broadcasts your player.
;; - Tracks download as the best audio, or videos, with yt-dlp, into
;;   the folders of the r4 command line, `r4-download-directory'.
;; - Follow channels, read your feed, and edit your channels and tracks
;;   after `r4-login'.
;; - Local channels live in files, without a radio4000.com account,
;;   and sync with radio4000.com.
;;
;; Requires the programs mpv, yt-dlp and ffmpeg.  See features.org.

;;; Code:

(require 'r4-models)
(require 'r4-api)
(require 'r4-files)
(require 'r4-download)
(require 'r4-downloads)
(require 'r4-player)
(require 'r4-queue)
(require 'r4-player-window)
(require 'r4-history)
(require 'r4-tracks)
(require 'r4-channels)
(require 'r4-search)
(require 'r4-local)
(require 'r4-auth)
(require 'r4-sync)
(require 'r4-follows)
(require 'r4-edit)
(require 'r4-auto-radio)
(require 'r4-broadcast)
(require 'r4-broadcasting)
(require 'r4-describe)
(require 'r4-channel-page)
(require 'r4-home)
(require 'r4-explore)
(require 'r4-synced)
(require 'r4-bookmark)
(require 'r4-embark)
(require 'r4-menus)

;;;###autoload
(defcustom r4-define-global-key-bindings t
  "When non-nil, bind \\`C-c r' to `r4' and \\`C-c 4' to `r4-command-map'.
Like magit binds \\`C-x g', each key is only bound if it is free once
your init file ran.  Set it to nil before r4 loads, to choose your
own keys."
  :type 'boolean
  :group 'r4)

;;;###autoload
(progn
  (defun r4--maybe-define-global-key-bindings ()
    "Bind the global keys of r4 that are free, see `r4-define-global-key-bindings'."
    (when r4-define-global-key-bindings
      (pcase-dolist (`(,key . ,command) '(("C-c r" . r4) ("C-c 4" . r4-command-map)))
        (unless (keymap-lookup (current-global-map) key)
          (keymap-global-set key command)))))
  (if after-init-time
      (r4--maybe-define-global-key-bindings)
    (add-hook 'after-init-hook #'r4--maybe-define-global-key-bindings t)))

;;;###autoload (autoload 'r4-command-map "r4" nil nil 'keymap)
(defvar-keymap r4-command-map
  :doc "Keys of r4 from anywhere, under a prefix, \\`C-c 4' by default.
\\`C-c 4 SPC' pauses, \\`C-c 4 n' plays the next track, \\`C-c 4 r' opens
the home buffer.  See `r4-define-global-key-bindings'."
  :prefix 'r4-command-map
  "r" #'r4
  "?" #'r4-menu
  "SPC" #'r4-player-toggle-pause
  "n" #'r4-player-next
  "p" #'r4-player-previous
  "f" #'r4-player-seek-forward
  "b" #'r4-player-seek-backward
  "+" #'r4-player-volume-up
  "-" #'r4-player-volume-down
  "k" #'r4-player-stop
  "Z" #'r4-player-toggle-shuffle
  "V" #'r4-player-toggle-video
  "v" #'r4-player-window
  "q" #'r4-queue
  "w" #'r4-player-share-menu
  "l" #'r4-broadcast-toggle
  "j" #'r4-channel-page
  "s" #'r4-search
  "E" #'r4-explore
  "B" #'r4-broadcasts
  "H" #'r4-history
  "L" #'r4-downloads)

(define-obsolete-function-alias 'r4-status #'r4-channels "0.2.0")
(define-obsolete-function-alias 'list-r4-channels #'r4-channels "0.2.0")
(define-obsolete-function-alias 'r4-describe-channel-slug #'r4-open-channel "0.2.0")
(define-obsolete-function-alias 'r4-kill-player #'r4-player-stop "0.2.0")
(define-obsolete-function-alias 'r4-player-play-next-track #'r4-player-next "0.2.0")

(provide 'r4)

;;; r4.el ends here
