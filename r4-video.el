;;; r4-video.el --- Show Radio4000 videos in an Emacs frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; mpv shows videos inside an Emacs frame, with its --wid option, on
;; X and MS-Windows.  Elsewhere, such as Emacs built for Wayland only
;; (pgtk) or macOS, it shows them in its own window.  Deleting the
;; video frame turns videos off.

;;; Code:

(require 'r4-mpv)

(defcustom r4-player-video-frame t
  "When non-nil, show videos in an Emacs frame, else in a mpv window.
Emacs frames show videos on X and MS-Windows, including X Emacs on
Wayland, not on Emacs built for Wayland only (pgtk) or macOS."
  :type 'boolean
  :group 'r4)

(defcustom r4-player-video-frame-parameters
  '((name . "r4 video") (width . 80) (height . 24) (minibuffer . nil))
  "Parameters of the frame showing videos, see `make-frame'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'r4)

(defconst r4-video-buffer "*r4 video*"
  "Name of the buffer of the video frame, under the video.")

(defvar r4-video-frame-deleted-hook nil
  "Hook run after the video frame was deleted, other than by r4.")

(defvar r4-video--frame nil "The frame showing videos.")
(defvar r4-video--deleting nil "Non-nil while r4 deletes the video frame.")

(defvar-keymap r4-video-mode-map
  :doc "Keymap for `r4-video-mode', the player keys.")

(define-derived-mode r4-video-mode special-mode "r4 video"
  "Major mode of the buffer of the video frame, under the video.

\\{r4-video-mode-map}"
  (setq mode-line-format nil
        cursor-type nil))

(defun r4-video-frame-p ()
  "Return non-nil if videos show in an Emacs frame."
  (and r4-player-video-frame (memq window-system '(x w32))))

(defun r4-video-frame-id ()
  "Return the window id of the video frame, making the frame if needed."
  (unless (frame-live-p r4-video--frame)
    (let ((buffer (get-buffer-create r4-video-buffer)))
      (with-current-buffer buffer
        (r4-video-mode))
      (setq r4-video--frame (make-frame r4-player-video-frame-parameters))
      (set-window-buffer (frame-root-window r4-video--frame) buffer)
      (set-window-dedicated-p (frame-root-window r4-video--frame) t)))
  (string-to-number (frame-parameter r4-video--frame 'window-id)))

(defun r4-video--release ()
  "Turn off the video of mpv, and wait until its video output is closed.
Else mpv stops playing when the frame it draws in disappears."
  (when (r4-mpv-live-p)
    (r4-mpv-send "set_property" "vid" "no")
    (let ((deadline (+ (float-time) 1))
          (configured t))
      (while (and configured (< (float-time) deadline))
        (setq configured 'waiting)
        (r4-mpv-get-property "vo-configured" (lambda (value) (setq configured value)))
        (while (and (eq configured 'waiting) (< (float-time) deadline))
          (accept-process-output nil 0.05))))))

(defun r4-video-delete-frame ()
  "Delete the video frame, if any."
  (when (frame-live-p r4-video--frame)
    (r4-video--release)
    (let ((r4-video--deleting t))
      (delete-frame r4-video--frame t)))
  (setq r4-video--frame nil))

(defun r4-video--frame-deleted (frame)
  "Turn videos off when the video FRAME is deleted."
  (when (and (eq frame r4-video--frame) (not r4-video--deleting))
    (r4-video--release)
    (setq r4-video--frame nil)
    (run-hooks 'r4-video-frame-deleted-hook)))

(add-hook 'delete-frame-functions #'r4-video--frame-deleted)
(add-hook 'r4-mpv-exit-hook #'r4-video-delete-frame)

(provide 'r4-video)

;;; r4-video.el ends here
