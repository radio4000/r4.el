;;; r4-faces.el --- Faces of Radio4000 buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The faces of the r4 buffers.  They inherit standard faces, so they
;; follow the colors of your theme; customize them with
;; M-x customize-group RET r4-faces.

;;; Code:

(defgroup r4-faces nil
  "Faces of the Radio4000 buffers."
  :group 'r4
  :group 'faces)

(defface r4-slug '((t :inherit font-lock-keyword-face))
  "Face of channel slugs, such as @ko002."
  :group 'r4-faces)

(defface r4-name '((t :inherit bold))
  "Face of channel names."
  :group 'r4-faces)

(defface r4-title '((t :inherit default))
  "Face of track titles."
  :group 'r4-faces)

(defface r4-tag '((t :inherit font-lock-type-face))
  "Face of tags, such as #jazz."
  :group 'r4-faces)

(defface r4-date '((t :inherit font-lock-comment-face))
  "Face of dates and times."
  :group 'r4-faces)

(defface r4-number '((t :inherit font-lock-constant-face))
  "Face of numbers, such as durations and track counts."
  :group 'r4-faces)

(defface r4-description '((t :inherit font-lock-doc-face))
  "Face of descriptions."
  :group 'r4-faces)

(defface r4-heading '((t :inherit (font-lock-function-name-face bold)))
  "Face of headings, and of the field names of channel pages."
  :group 'r4-faces)

(defface r4-big-heading '((t :inherit r4-heading :height 1.4))
  "Face of the main heading of a buffer, such as a channel name."
  :group 'r4-faces)

(defface r4-playing '((t :inherit (success bold)))
  "Face of what plays: ▶."
  :group 'r4-faces)

(defface r4-downloaded '((t :inherit success))
  "Face of downloaded tracks and channels: ✓."
  :group 'r4-faces)

(defface r4-downloading '((t :inherit warning))
  "Face of what downloads: ↓."
  :group 'r4-faces)

(defface r4-failed '((t :inherit error))
  "Face of failed downloads: ✗."
  :group 'r4-faces)

(defface r4-followed '((t :inherit font-lock-string-face))
  "Face of followed channels: ♥."
  :group 'r4-faces)

(defface r4-synced '((t :inherit font-lock-builtin-face))
  "Face of synced channels: ⟳."
  :group 'r4-faces)

(defun r4-faces-status (text face)
  "Return TEXT in FACE, or a space if TEXT is nil."
  (if text (propertize text 'face face) " "))

(defun r4-faces-insert-key (key label command &optional on)
  "Insert KEY and a button LABEL running COMMAND, like in magit.
The button is highlighted when ON, such as a toggle that is on."
  (insert (propertize key 'face 'help-key-binding) " ")
  (insert-text-button label
                      'action (lambda (_) (call-interactively command))
                      'follow-link t
                      'face (if on 'success 'button)
                      'help-echo (documentation command t)))

(provide 'r4-faces)

;;; r4-faces.el ends here
