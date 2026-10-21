;;; r4-video.el --- Show Radio4000 videos in an Emacs window  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Videos show in an area of an Emacs window, the player window (see
;; r4-player-window.el), under the playing track.  mpv draws them with
;; its --wid option in a child frame without decorations, kept over that
;; area when it moves or resizes.  This works on X, including X Emacs
;; on Wayland, and MS-Windows.  Elsewhere, such as Emacs built for
;; Wayland only (pgtk) or macOS, or when `r4-player-video-pop-out' is
;; non-nil, mpv shows them in its own window.  Quitting the window of
;; the video turns videos off.

;;; Code:

(require 'r4-mpv)

(defcustom r4-player-video-pop-out nil
  "When non-nil, show videos in a mpv window, else in the player window.
Emacs windows show videos on X and MS-Windows, including X Emacs on
Wayland, not on Emacs built for Wayland only (pgtk) or macOS, where
videos always pop out.  Toggle it with `r4-player-toggle-video-pop-out'."
  :type 'boolean
  :group 'r4)

(defvar r4-video-show-function nil
  "Function showing the window where videos show, see `r4-video-area-function'.")

(defvar r4-video-area-function nil
  "Function returning the area of videos, or nil when it is not shown.
The area is a list of its window and its pixel edges in its frame,
LEFT TOP RIGHT BOTTOM.")

(defvar r4-video-window-quit-hook nil
  "Hook run after the window of the video was quit, to turn videos off.")

(defconst r4-video--surface-buffer " *r4 video surface*"
  "Name of the buffer of the child frame mpv draws in.")

(defvar r4-video--frame nil "The child frame mpv draws in.")
(defvar r4-video--area nil "The last area of the video, to only move it when it changes.")

(defun r4-video-window-p ()
  "Return non-nil if videos show in an Emacs window."
  (and (not r4-player-video-pop-out) r4-video-show-function
       (memq window-system '(x w32))))

(defun r4-video-mpv-arguments ()
  "Return the arguments of mpv to draw videos in Emacs windows.
On X, mpv draws in Emacs with X, also when Wayland runs, where it
would use Wayland, which cannot draw in Emacs."
  (when (eq window-system 'x)
    '("--gpu-context=x11egl")))

(defun r4-video--make-frame (parent)
  "Make the child frame of PARENT where mpv draws."
  (let ((frame (make-frame
                `((parent-frame . ,parent)
                  (minibuffer . nil)
                  (undecorated . t)
                  (no-accept-focus . t)
                  (no-other-frame . t)
                  (no-special-glyphs . t)
                  (skip-taskbar . t)
                  (unsplittable . t)
                  (desktop-dont-save . t)
                  (internal-border-width . 0)
                  (child-frame-border-width . 0)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (vertical-scroll-bars . nil)
                  (horizontal-scroll-bars . nil)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (tab-bar-lines . 0)
                  (background-color . "black")
                  (visibility . nil)))))
    (with-current-buffer (get-buffer-create r4-video--surface-buffer)
      (setq mode-line-format nil cursor-type nil))
    (set-window-buffer (frame-root-window frame) r4-video--surface-buffer)
    (set-window-dedicated-p (frame-root-window frame) t)
    frame))

(defun r4-video-place ()
  "Keep the child frame over the video area, or turn videos off."
  (when (frame-live-p r4-video--frame)
    (pcase (funcall r4-video-area-function)
      ((and `(,window . ,(and edges `(,left ,top ,right ,bottom)))
            (guard (eq (window-frame window) (frame-parent r4-video--frame))))
       (unless (and (equal edges r4-video--area) (frame-visible-p r4-video--frame))
         (setq r4-video--area edges)
         (modify-frame-parameters r4-video--frame
                                  `((left . ,left) (top . ,top)
                                    (width . (text-pixels . ,(max 1 (- right left))))
                                    (height . (text-pixels . ,(max 1 (- bottom top))))))
         (make-frame-visible r4-video--frame)))
      (_ (r4-video-quit)
         (run-hooks 'r4-video-window-quit-hook)))))

(defun r4-video-window-id ()
  "Show the video area and return the id of the window mpv draws in."
  (let ((window (funcall r4-video-show-function)))
    (unless (frame-live-p r4-video--frame)
      (setq r4-video--frame (r4-video--make-frame (window-frame window)))
      (add-hook 'window-state-change-functions #'r4-video--state-changed))
    (r4-video-place)
    (string-to-number (frame-parameter r4-video--frame 'window-id))))

(defun r4-video--state-changed (_frame)
  "Keep the video over its area, after windows changed."
  (r4-video-place))

(defun r4-video--release ()
  "Turn off the video of mpv, and wait until its video output is closed.
Else mpv stops playing when the window it draws in disappears."
  (when (r4-mpv-live-p)
    (r4-mpv-send "set_property" "vid" "no")
    (let ((deadline (+ (float-time) 1))
          (configured t))
      (while (and configured (< (float-time) deadline))
        (setq configured 'waiting)
        (r4-mpv-get-property "vo-configured" (lambda (value) (setq configured value)))
        (while (and (eq configured 'waiting) (< (float-time) deadline))
          (accept-process-output nil 0.05))))))

(defun r4-video-showing-p ()
  "Return non-nil if a video shows in an Emacs window."
  (frame-live-p r4-video--frame))

(defun r4-video-quit ()
  "Stop showing videos, deleting the child frame."
  (remove-hook 'window-state-change-functions #'r4-video--state-changed)
  ;; Also when it pops out, for mpv to draw in a new window next time.
  (r4-video--release)
  (when (frame-live-p r4-video--frame)
    (delete-frame r4-video--frame t))
  (setq r4-video--frame nil
        r4-video--area nil))

(add-hook 'r4-mpv-exit-hook #'r4-video-quit)

(provide 'r4-video)

;;; r4-video.el ends here
