;;; r4-embark.el --- Radio4000 actions for Embark  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; With Embark (https://github.com/oantolin/embark), act on the
;; channel at point in any channel completion, such as the one of
;; `r4-open-channel': open, play, tune in, describe, download it…
;; Nothing happens without Embark.

;;; Code:

(require 'r4-channels)
(require 'r4-search)
(require 'r4-local)
(require 'r4-auto-radio)
(require 'r4-describe)

(defvar-keymap r4-embark-channel-map
  :doc "Embark actions on a Radio4000 channel."
  "o" #'r4-open-channel
  "p" #'r4-play-channel
  "R" #'r4-auto-radio
  "i" #'r4-describe-channel
  "d" #'r4-download-channel
  "@" #'r4-mentions
  "S" #'r4-local-save-channel)

(defvar embark-keymap-alist)

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(r4-channel . r4-embark-channel-map)))

(provide 'r4-embark)

;;; r4-embark.el ends here
