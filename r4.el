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
;;   tracks of a channel, `r4-search' searches tracks, as in
;;   "@ko002 #jazz miles".
;; - Tracks are played with mpv, from disk when downloaded.
;; - Tracks are downloaded as high quality audio with yt-dlp into
;;   `r4-download-directory' (~/Music/radio4000/ by default).
;; - Local channels live in files, without a radio4000.com account,
;;   and sync with radio4000.com after `r4-login'.
;;
;; Requires the programs mpv, yt-dlp and ffmpeg.

;;; Code:

(require 'r4-models)
(require 'r4-api)
(require 'r4-files)
(require 'r4-download)
(require 'r4-downloads)
(require 'r4-player)
(require 'r4-queue)
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
(require 'r4-explore)
(require 'r4-menus)

(define-obsolete-function-alias 'r4-status #'r4-channels "0.2.0")
(define-obsolete-function-alias 'list-r4-channels #'r4-channels "0.2.0")
(define-obsolete-function-alias 'r4-describe-channel-slug #'r4-open-channel "0.2.0")
(define-obsolete-function-alias 'r4-kill-player #'r4-player-stop "0.2.0")
(define-obsolete-function-alias 'r4-player-play-next-track #'r4-player-next "0.2.0")

(provide 'r4)

;;; r4.el ends here
