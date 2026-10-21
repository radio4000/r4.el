;;; r4-describe.el --- Describe Radio4000 channels and tracks  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Help buffers describing a track, with buttons for what you can do
;; with it.  Channels have pages, see r4-channel-page.el.

;;; Code:

(require 'cl-lib)
(require 'help-mode)
(require 'r4-models)
(require 'r4-api)
(require 'r4-files)
(require 'r4-tracks)
(require 'r4-channels)
(require 'r4-search)
(require 'r4-follows)
(require 'r4-auto-radio)

(defun r4-describe--field (label value)
  "Insert the field LABEL with VALUE, if VALUE is not empty."
  (when (and value (not (equal value "")))
    (insert (format "%-13s" (concat label ":")) value "\n")))

(defun r4-describe--link (label url)
  "Insert the field LABEL with a button opening URL, if non-nil."
  (when (and url (not (equal url "")))
    (insert (format "%-13s" (concat label ":")))
    (insert-button url 'type 'help-url 'help-args (list url))
    (insert "\n")))

(defun r4-describe--buttons (buttons)
  "Insert BUTTONS, a list of (LABEL . FUNCTION), on lines of buttons."
  (let ((column 0))
    (pcase-dolist (`(,label . ,function) buttons)
      (when (> (+ column (length label) 4) fill-column)
        (insert "\n")
        (setq column 0))
      (insert-text-button (format "[%s]" label) 'action (lambda (_) (funcall function)))
      (insert "  ")
      (cl-incf column (+ (length label) 4))))
  (insert "\n"))

;;;###autoload
(defun r4-describe-track (track)
  "Describe TRACK, an `r4-track'."
  (with-help-window (format "*r4 track %s*" (r4-one-line (r4-track-title track)))
    (with-current-buffer standard-output
      (insert (propertize (or (r4-track-title track) "") 'face 'bold) "\n\n")
      (when-let* ((description (r4-track-description track)))
        (unless (string-blank-p description)
          (insert description "\n\n")))
      (r4-describe--field "Channel" (concat "@" (r4-track-slug track)))
      (r4-describe--field "Tags" (mapconcat (lambda (tag) (concat "#" tag)) (r4-track-tags track) " "))
      (r4-describe--field "Duration" (r4-format-duration (r4-track-duration track)))
      (r4-describe--field "Added" (r4-format-date (r4-track-created-at track)))
      (r4-describe--field "Updated" (r4-format-date (r4-track-updated-at track)))
      (r4-describe--field "Provider" (r4-track-provider track))
      (r4-describe--link "Media" (r4-track-url track))
      (r4-describe--link "Discogs" (r4-track-discogs-url track))
      (unless (r4-track-local-p track)
        (r4-describe--link "Web" (r4-track-web-url track)))
      (dolist (file (r4-files-track-files track))
        (r4-describe--field "File" file))
      (insert "\n")
      (r4-describe--buttons
       `(("Play" . ,(lambda () (r4-player-play (list track))))
         ("Play next" . ,(lambda () (r4-player-play-next (list track))))
         ("Add to queue" . ,(lambda () (r4-player-add-to-queue (list track))))
         ("Download" . ,(lambda () (r4-download-tracks (list track))))
         ("Open channel" . ,(lambda () (r4-tracks--visit-channel track)))
         ("Copy media URL" . ,(lambda () (kill-new (r4-track-url track)))))))))

(defun r4-tracks-describe ()
  "Describe the track at point."
  (interactive)
  (r4-describe-track (r4-tracks--at-point)))

(keymap-set r4-tracks-mode-map "i" #'r4-tracks-describe)

(provide 'r4-describe)

;;; r4-describe.el ends here
