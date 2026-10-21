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
;; - `r4' opens the home buffer, like the home page of radio4000.com,
;;   where ? opens the menu of all the commands, `r4-menu', and ? in
;;   any list the menu of that list.
;; - `r4-channels', `r4-explore' and `r4-search' browse channels and
;;   tracks, with the search language of radio4000.com, as in
;;   "@ko002 #jazz miles".
;; - Tracks play with mpv, from disk when downloaded; `r4-auto-radio'
;;   plays the same live radio as radio4000.com, and `r4-broadcasts'
;;   lists live broadcasts.
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
(require 'r4-describe)
(require 'r4-channel-page)
(require 'r4-home)
(require 'r4-explore)
(require 'r4-synced)
(require 'r4-bookmark)
(require 'r4-embark)
(require 'r4-menus)

(define-obsolete-function-alias 'r4-status #'r4-channels "0.2.0")
(define-obsolete-function-alias 'list-r4-channels #'r4-channels "0.2.0")
(define-obsolete-function-alias 'r4-describe-channel-slug #'r4-open-channel "0.2.0")
(define-obsolete-function-alias 'r4-kill-player #'r4-player-stop "0.2.0")
(define-obsolete-function-alias 'r4-player-play-next-track #'r4-player-next "0.2.0")

(provide 'r4)

;;; r4.el ends here
