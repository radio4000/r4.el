;;; r4-describe.el --- Describe Radio4000 channels and tracks  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Help buffers describing a channel, with its image, and a track, with
;; buttons for what you can do with them.

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

(defcustom r4-channel-image-size 250
  "Size in pixels of the channel image in channel descriptions.
0 hides the image."
  :type 'natnum
  :group 'r4)

(defun r4-describe--insert-image (channel)
  "Insert the image of CHANNEL at point, once it is fetched."
  (when-let* (((display-images-p))
              ((> r4-channel-image-size 0))
              (url (r4-channel-image-url channel r4-channel-image-size)))
    (let ((buffer (current-buffer))
          (marker (point-marker)))
      (r4-api-fetch-image
       url
       (lambda (image)
         (when (and image (buffer-live-p buffer))
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (save-excursion
                 (goto-char marker)
                 (insert-image image "[image]")
                 (insert "\n\n"))))))))))

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
(defun r4-describe-channel (channel)
  "Describe CHANNEL, an `r4-channel' or a slug."
  (interactive (list (r4-read-channel "Describe channel: @")))
  (if (stringp channel)
      (r4-api-channel channel
                      (lambda (found)
                        (if found
                            (r4-describe-channel found)
                          (message "No channel @%s" channel))))
    (let ((slug (r4-channel-slug channel)))
      (with-help-window (format "*r4 @%s info*" slug)
        (with-current-buffer standard-output
          (r4-describe--insert-image channel)
          (insert (propertize (or (r4-channel-name channel) "") 'face 'bold) "  @" slug "\n\n")
          (when-let* ((description (r4-channel-description channel)))
            (insert description "\n\n"))
          (r4-describe--field "Tracks" (format "%s" (or (r4-channel-track-count channel) 0)))
          (r4-describe--field "Created" (r4-format-date (r4-channel-created-at channel)))
          (r4-describe--field "Updated" (r4-format-date (r4-channel-updated-at channel)))
          (r4-describe--field "Last track" (r4-format-date (r4-channel-latest-track-at channel)))
          (r4-describe--link "Link" (r4-channel-url channel))
          (r4-describe--link "Web" (r4-channel-web-url channel))
          (r4-describe--link "RSS" (r4-channel-rss-url channel))
          (r4-describe--link "Map" (r4-channel-map-url channel))
          (r4-describe--field "Downloads" (r4-files-channel-directory slug))
          (insert "\n")
          (r4-describe--buttons
           `(("Open tracks" . ,(lambda () (r4-open-channel slug)))
             ("Play" . ,(lambda () (r4-play-channel slug)))
             ("Auto-radio" . ,(lambda () (r4-auto-radio slug)))
             ("Download" . ,(lambda () (r4-download-channel slug)))
             ("Mentions" . ,(lambda () (r4-mentions slug)))
             ("Following" . ,(lambda () (r4-following slug)))
             ("Followers" . ,(lambda () (r4-followers slug)))
             ("Save locally" . ,(lambda () (r4-local-save-channel slug))))))))))

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
      (r4-describe--link "Web" (r4-track-web-url track))
      (dolist (file (r4-files-track-files track))
        (r4-describe--field "File" file))
      (insert "\n")
      (r4-describe--buttons
       `(("Play" . ,(lambda () (r4-player-play (list track))))
         ("Play next" . ,(lambda () (r4-player-play-next (list track))))
         ("Add to queue" . ,(lambda () (r4-player-add-to-queue (list track))))
         ("Download" . ,(lambda () (r4-download-tracks (list track))))
         ("Open channel" . ,(lambda () (r4-open-channel (r4-track-slug track))))
         ("Copy media URL" . ,(lambda () (kill-new (r4-track-url track)))))))))

(defun r4-tracks-describe ()
  "Describe the track at point."
  (interactive)
  (r4-describe-track (r4-tracks--at-point)))

(keymap-set r4-tracks-mode-map "i" #'r4-tracks-describe)

(provide 'r4-describe)

;;; r4-describe.el ends here
