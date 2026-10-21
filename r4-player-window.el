;;; r4-player-window.el --- The Radio4000 player in a window  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The player in its own window, at the top of the frame by default:
;; the playing track, its channel, the elapsed time and duration, and
;; a progress bar to click to seek, and the video under them, see
;; r4-video.el.  `r4-player-window', v, shows or hides it.  The player
;; keys work in it.

;;; Code:

(require 'r4-models)
(require 'r4-player)
(require 'r4-video)

(defconst r4-player-window-buffer "*r4 player*"
  "Name of the buffer of the player window.")

(defcustom r4-player-window-action
  '(display-buffer-in-side-window
    (side . top)
    (slot . 0)
    (window-height . fit-window-to-buffer)
    (preserve-size . (nil . t))
    (dedicated . t)
    (window-parameters (no-other-window . t) (no-delete-other-windows . t)))
  "Action displaying the player window, see `display-buffer'.
By default, a side window at the top of the frame.  An entry for
`r4-player-window-buffer' in `display-buffer-alist' overrides it."
  :type 'sexp
  :group 'r4)

(defcustom r4-player-window-video-height 0.4
  "Height of the player window showing a video, a fraction of the frame."
  :type 'number
  :group 'r4)

(defface r4-player-window-played '((t :inherit success))
  "Face of the played part of the progress bar."
  :group 'r4)

(defface r4-player-window-remaining '((t :inherit shadow))
  "Face of the remaining part of the progress bar."
  :group 'r4)

(defvar-keymap r4-player-window-button-map
  :doc "Mouse keys of the pause button."
  "<mouse-1>" #'r4-player-toggle-pause
  "RET" #'r4-player-toggle-pause)

(defun r4-player-window-seek-at-mouse (event)
  "Seek to the position of the progress bar clicked in EVENT."
  (interactive "e")
  (when-let* ((percent (get-text-property (posn-point (event-start event)) 'r4-percent)))
    (r4-player-seek-percent percent)))

(defvar-keymap r4-player-window-bar-map
  :doc "Mouse keys of the progress bar."
  "<mouse-1>" #'r4-player-window-seek-at-mouse)

(defun r4-player-window--bar (width)
  "Return the progress bar of the playing track, WIDTH characters wide."
  (let* ((duration (r4-player-duration))
         (played (if (and duration (> duration 0) r4-player-time-pos)
                     (min width (round (* width (/ r4-player-time-pos duration))))
                   0)))
    (apply #'concat
           (mapcar (lambda (column)
                     (propertize (if (< column played) "━" "─")
                                 'face (if (< column played)
                                           'r4-player-window-played
                                         'r4-player-window-remaining)
                                 'r4-percent (/ (* 100.0 column) width)
                                 'mouse-face 'highlight
                                 'help-echo "mouse-1: seek here"
                                 'keymap r4-player-window-bar-map))
                   (number-sequence 0 (1- width))))))

(defun r4-player-window--options ()
  "Return the options of the player shown in its window."
  (string-join (delq nil (list (and r4-player-shuffle "shuffle")
                               (and r4-player-video
                                    (if (r4-video-window-p) "video" "video, popped out"))))
               " "))

(defun r4-player-window--insert (width)
  "Insert the player, WIDTH characters wide."
  (if-let* ((track (and (r4-player-live-p) (r4-player-current))))
      (let* ((button (propertize (if r4-player-paused "⏸" "▶")
                                 'mouse-face 'highlight
                                 'help-echo "mouse-1: pause or resume"
                                 'keymap r4-player-window-button-map))
             (info (concat "@" (r4-track-slug track)
                           (if r4-player-label (format " [%s]" r4-player-label) "")))
             (time (r4-player-time-string))
             (options (r4-player-window--options))
             (bar-width (- width (string-width info) (string-width time)
                           (string-width options) 5)))
        (insert button " "
                (propertize (truncate-string-to-width (r4-one-line (r4-track-title track))
                                                      (- width 2) nil nil "…")
                            'face 'bold)
                "\n" info " " time " ")
        (when (> bar-width 4)
          (insert (r4-player-window--bar bar-width) " "))
        (insert options))
    (insert "Nothing is playing\n"
            (propertize "RET in a list plays a track, ? shows the keys" 'face 'shadow))))

(defun r4-player-window--refresh ()
  "Redraw the player window, if shown."
  (when-let* ((buffer (get-buffer r4-player-window-buffer))
              (window (get-buffer-window buffer t)))
    (with-current-buffer buffer
      (r4-player-window--render (1- (window-max-chars-per-line window))))
    (r4-player-window--fit window)))

(defun r4-player-window--fit (window)
  "Fit the side WINDOW of the player to its text, and the video if any."
  (when (window-parameter window 'window-side)
    (let ((height (and r4-player-video (r4-video-window-p)
                       (floor (* r4-player-window-video-height
                                 (frame-height (window-frame window)))))))
      (fit-window-to-buffer window height height))))

(defun r4-player-window--show ()
  "Show the player window and return it."
  (or (get-buffer-window r4-player-window-buffer)
      (progn
        (with-current-buffer (get-buffer-create r4-player-window-buffer)
          (unless (derived-mode-p 'r4-player-window-mode)
            (r4-player-window-mode))
          ;; Its content, for the window to fit it.
          (r4-player-window--render (frame-width)))
        (prog1 (display-buffer r4-player-window-buffer r4-player-window-action)
          (r4-player-window--refresh)))))

(defun r4-player-window--video-area ()
  "Return the player window and the pixel edges of its video area, or nil.
The video is under the text of the player."
  (when-let* ((window (get-buffer-window r4-player-window-buffer)))
    (pcase-let ((`(,left ,top ,right ,bottom) (window-inside-pixel-edges window)))
      (list window left (+ top (cdr (window-text-pixel-size window))) right bottom))))

(setq r4-video-show-function #'r4-player-window--show
      r4-video-area-function #'r4-player-window--video-area)

(defun r4-player-window--render (width)
  "Replace the content of the current buffer by the player, WIDTH wide."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (r4-player-window--insert width)
    (goto-char (point-min))))

(add-hook 'r4-player-changed-hook #'r4-player-window--refresh)
(add-hook 'r4-player-queue-changed-hook #'r4-player-window--refresh)

(defvar-keymap r4-player-window-mode-map
  :doc "Keymap for `r4-player-window-mode'."
  :parent (make-composed-keymap r4-player-map special-mode-map)
  "v" #'r4-player-window)

(define-derived-mode r4-player-window-mode special-mode "r4 player"
  "Major mode of the Radio4000 player window.

\\{r4-player-window-mode-map}"
  (setq mode-line-format nil
        cursor-type nil
        truncate-lines t)
  (add-hook 'window-size-change-functions
            (lambda (_) (r4-player-window--refresh))
            nil t))

;;;###autoload
(defun r4-player-window ()
  "Show or hide the player window."
  (interactive)
  (if-let* ((window (get-buffer-window r4-player-window-buffer)))
      (quit-window nil window)
    (r4-player-window--show)))

(keymap-set r4-player-map "v" #'r4-player-window)

(provide 'r4-player-window)

;;; r4-player-window.el ends here
