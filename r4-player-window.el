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
(require 'r4-faces)

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

(defcustom r4-player-window-text-width 70
  "Width in columns of the player beside a video.
In a window too narrow for both, the video shows under the player."
  :type 'natnum
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

(defun r4-player-window--bar-length (width)
  "Return how many characters of the bar fit in WIDTH columns.
Fonts often draw its characters wider than a column."
  (let ((column (string-pixel-width " "))
        (char (max (string-pixel-width "━") (string-pixel-width "─"))))
    (if (and (> column 0) (> char column))
        (floor (* width column) char)
      width)))

(defun r4-player-window--bar (columns)
  "Return the progress bar of the playing track, at most COLUMNS wide."
  (let* ((width (r4-player-window--bar-length columns))
         (duration (r4-player-duration))
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

(defcustom r4-player-window-show-controls nil
  "When non-nil, show the controls of the player and their keys in its window.
Else the window is compact, and ? shows the keys.  The home buffer
always shows them."
  :type 'boolean
  :group 'r4)

(defconst r4-player-controls
  '(("M-p" "previous" r4-player-previous)
    ("SPC" "pause" r4-player-toggle-pause r4-player-paused "play")
    ("M-n" "next" r4-player-next)
    ("M-b" "-10s" r4-player-seek-backward)
    ("M-f" "+10s" r4-player-seek-forward)
    ("M-<down>" "quieter" r4-player-volume-down)
    ("M-<up>" "louder" r4-player-volume-up)
    ("Z" "shuffle" r4-player-toggle-shuffle r4-player-shuffle)
    ("V" "video" r4-player-toggle-video r4-player-video)
    ("Q" "queue" r4-queue)
    ("b" "go live" r4-broadcast-toggle r4-broadcasting "stop live")
    ("k" "stop" r4-player-stop))
  "Controls of the player: key, label, command, and for a toggle its variable.
A toggle that is on is highlighted, with its last label if any, such as
\"stop live\" while broadcasting.  The keys work in every r4 buffer, see
`r4-player-map'.")

(defun r4-player-window--insert-controls (controls)
  "Insert CONTROLS, a list like `r4-player-controls'."
  (pcase-dolist (`(,key ,label ,command ,variable ,on-label) controls)
    (let ((on (and variable (boundp variable) (symbol-value variable))))
      (r4-faces-insert-key key (or (and on on-label) label) command on)
      (insert "  "))))

(defun r4-player-window--states ()
  "Return the options of the player that are on, such as shuffle."
  (mapconcat #'identity
             (delq nil (list (and r4-player-shuffle (propertize "shuffle" 'face 'success))
                             (and r4-player-video (propertize "video" 'face 'success))
                             (and (bound-and-true-p r4-broadcasting)
                                  (propertize "live" 'face 'error))))
             " "))

(defun r4-player-window--insert (width &optional controls)
  "Insert the player, WIDTH characters wide, with its CONTROLS if non-nil."
  (if-let* ((track (and (r4-player-live-p) (r4-player-current))))
      (let* ((button (propertize (if r4-player-paused "⏸" "▶")
                                 'mouse-face 'highlight
                                 'help-echo "mouse-1: pause or resume"
                                 'keymap r4-player-window-button-map))
             (info (concat "@" (r4-track-slug track)
                           (if r4-player-label (format " [%s]" r4-player-label) "")))
             (time (r4-player-time-string))
             (states (if controls "" (r4-player-window--states)))
             (bar-width (- width (string-width info) (string-width time)
                           (string-width states) 4)))
        (insert button " "
                (propertize (truncate-string-to-width (r4-one-line (r4-track-title track))
                                                      (- width 2) nil nil "…")
                            'face 'r4-title)
                "\n" info " " time " ")
        (when (> bar-width 4)
          (insert (r4-player-window--bar bar-width)))
        (unless (string-empty-p states)
          (insert " " states))
        (when controls
          (insert "\n")
          (r4-player-window--insert-controls r4-player-controls)))
    (insert "Nothing is playing: RET in a list of tracks plays them")
    (when controls
      (insert "\n")
      (r4-player-window--insert-controls
       (seq-filter (lambda (control) (memq (nth 2 control) '(r4-player-toggle-shuffle
                                                             r4-player-toggle-video
                                                             r4-queue)))
                   r4-player-controls)))))

(defun r4-player-window--video-p ()
  "Return non-nil if the player window shows a video."
  (and r4-player-video (r4-video-window-p)))

(defun r4-player-window--video-width (window)
  "Return the width in pixels of a video as high as WINDOW."
  (round (* (or r4-player-video-aspect (/ 16.0 9))
            (window-body-height window t))))

(defun r4-player-window--beside-p (window)
  "Return non-nil if WINDOW shows the video beside the player, else under."
  (and (r4-player-window--video-p)
       (>= (- (window-body-width window t) (r4-player-window--video-width window))
           (* r4-player-window-text-width (frame-char-width (window-frame window))))))

(defun r4-player-window--refresh ()
  "Redraw the player window, if shown, and place its video."
  (when-let* ((buffer (get-buffer r4-player-window-buffer))
              (window (get-buffer-window buffer t)))
    (r4-player-window--fit window)
    (with-current-buffer buffer
      (r4-player-window--render
       (1- (if (r4-player-window--beside-p window)
               (/ (- (window-body-width window t) (r4-player-window--video-width window))
                  (frame-char-width (window-frame window)))
             (window-max-chars-per-line window)))))
    (when (r4-video-showing-p)
      (r4-video-place))))

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
The video is beside the player in a wide window, else under it, as
wide as its aspect ratio allows."
  (when-let* ((window (get-buffer-window r4-player-window-buffer)))
    (pcase-let ((`(,left ,top ,right ,bottom) (window-inside-pixel-edges window)))
      (if (r4-player-window--beside-p window)
          (list window (- right (r4-player-window--video-width window)) top right bottom)
        (let* ((top (+ top (cdr (window-text-pixel-size window))))
               (width (min (- right left)
                           (round (* (or r4-player-video-aspect (/ 16.0 9)) (- bottom top)))))
               (left (+ left (/ (- right left width) 2))))
          (list window left top (+ left width) bottom))))))

(setq r4-video-show-function #'r4-player-window--show
      r4-video-area-function #'r4-player-window--video-area)

(defun r4-player-window--render (width)
  "Replace the content of the current buffer by the player, WIDTH wide."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (r4-player-window--insert width r4-player-window-show-controls)
    (goto-char (point-min))))

(add-hook 'r4-player-changed-hook #'r4-player-window--refresh)
(add-hook 'r4-player-queue-changed-hook #'r4-player-window--refresh)

(defvar-keymap r4-player-window-mode-map
  :doc "Keymap for `r4-player-window-mode'."
  :parent (make-composed-keymap r4-player-map special-mode-map)
  "v" #'r4-player-window
  ;; The player's own views: share the playing track.
  "w" #'r4-player-copy-url
  "W" #'r4-player-copy-web-url
  "o" #'r4-player-browse-current
  "O" #'r4-player-browse-media)

(define-derived-mode r4-player-window-mode special-mode "r4 player"
  "Major mode of the Radio4000 player window.

\\{r4-player-window-mode-map}"
  ;; A slim mode line, separating it from the windows under it.
  (setq mode-line-format
        `(" " (:propertize "r4 player" face bold)
          "  " (:propertize ,(substitute-command-keys
                              "\\<r4-player-window-mode-map>\\[r4-player-menu] keys  \\[r4-player-window] hide")
                            face shadow))
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
