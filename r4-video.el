;;; r4-video.el --- Show Radio4000 videos in an Emacs window  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Videos show in an Emacs window, of the buffer `r4-video-buffer'.
;; mpv draws them with its --wid option in a child frame without
;; decorations, kept over that window when it moves or resizes.  This
;; works on X, including X Emacs on Wayland, and MS-Windows.  Elsewhere,
;; such as Emacs built for Wayland only (pgtk) or macOS, mpv shows them
;; in its own window.  Quitting the video window turns videos off.

;;; Code:

(require 'r4-mpv)

(defcustom r4-player-video-window t
  "When non-nil, show videos in an Emacs window, else in a mpv window.
Emacs windows show videos on X and MS-Windows, including X Emacs on
Wayland, not on Emacs built for Wayland only (pgtk) or macOS."
  :type 'boolean
  :group 'r4)

(defcustom r4-player-video-window-action
  '(display-buffer-in-side-window (side . right) (window-width . 0.4))
  "Action displaying the video window, see `display-buffer'.
By default, a side window at the right of the frame.  An entry for
`r4-video-buffer' in `display-buffer-alist' overrides it."
  :type 'sexp
  :group 'r4)

(defconst r4-video-buffer "*r4 video*"
  "Name of the buffer of the video window.")

(defconst r4-video--surface-buffer " *r4 video surface*"
  "Name of the buffer of the child frame mpv draws in.")

(defvar r4-video-window-quit-hook nil
  "Hook run after the video window was quit, to turn videos off.")

(defvar r4-video--frame nil "The child frame mpv draws in.")

(defvar-keymap r4-video-mode-map
  :doc "Keymap for `r4-video-mode', the player keys.")

(define-derived-mode r4-video-mode special-mode "r4 video"
  "Major mode of the video window.

\\{r4-video-mode-map}"
  (setq cursor-type nil))

(defun r4-video-window-p ()
  "Return non-nil if videos show in an Emacs window."
  (and r4-player-video-window (memq window-system '(x w32))))

(defun r4-video-mpv-arguments ()
  "Return the arguments of mpv to draw videos in Emacs windows.
On X, mpv draws in Emacs with X, also when Wayland runs, where it
would use Wayland, which cannot draw in Emacs."
  (when (and (r4-video-window-p) (eq window-system 'x))
    '("--gpu-context=x11egl")))

(defun r4-video--window ()
  "Return the window showing the video buffer, or nil."
  (when-let* ((buffer (get-buffer r4-video-buffer)))
    (get-buffer-window buffer)))

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

(defun r4-video--place ()
  "Keep the child frame over the video window, or turn videos off."
  (when (frame-live-p r4-video--frame)
    (if-let* ((window (r4-video--window))
              ((eq (window-frame window) (frame-parent r4-video--frame))))
        (pcase-let ((`(,left ,top ,right ,bottom) (window-inside-pixel-edges window)))
          (modify-frame-parameters r4-video--frame
                                   `((left . ,left) (top . ,top)
                                     (width . (text-pixels . ,(- right left)))
                                     (height . (text-pixels . ,(- bottom top)))))
          (make-frame-visible r4-video--frame))
      (r4-video-quit)
      (run-hooks 'r4-video-window-quit-hook))))

(defun r4-video-window-id ()
  "Show the video window and return the id mpv draws in."
  (with-current-buffer (get-buffer-create r4-video-buffer)
    (unless (derived-mode-p 'r4-video-mode)
      (r4-video-mode)))
  (let ((window (or (r4-video--window)
                    (display-buffer r4-video-buffer r4-player-video-window-action))))
    (unless (frame-live-p r4-video--frame)
      (setq r4-video--frame (r4-video--make-frame (window-frame window)))
      (add-hook 'window-state-change-functions #'r4-video--state-changed))
    (r4-video--place)
    (string-to-number (frame-parameter r4-video--frame 'window-id))))

(defun r4-video--state-changed (_frame)
  "Keep the video over its window, after windows changed."
  (r4-video--place))

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

(defun r4-video-quit ()
  "Stop showing videos in Emacs: delete the child frame, quit the window."
  (remove-hook 'window-state-change-functions #'r4-video--state-changed)
  (when (frame-live-p r4-video--frame)
    (r4-video--release)
    (delete-frame r4-video--frame t))
  (setq r4-video--frame nil)
  (when-let* ((window (r4-video--window)))
    (quit-window nil window)))

(add-hook 'r4-mpv-exit-hook #'r4-video-quit)

(provide 'r4-video)

;;; r4-video.el ends here
