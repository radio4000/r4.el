;;; r4-player.el --- Play Radio4000 tracks with mpv  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A queue of tracks played by a single idle mpv process, controlled
;; through its JSON IPC socket.  Downloaded tracks are played from
;; disk, others are streamed.  When a track ends, the next one plays.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'r4-models)
(require 'r4-files)
(require 'r4-mpv)
(require 'r4-video)

(defcustom r4-player-program "mpv"
  "Program used to play tracks."
  :type 'string
  :group 'r4)

(defcustom r4-player-arguments nil
  "Extra arguments passed to `r4-player-program'."
  :type '(repeat string)
  :group 'r4)

(defcustom r4-player-environment '("LIBGL_ALWAYS_SOFTWARE")
  "Changes to the environment of `r4-player-program'.
Each is \"NAME=VALUE\" to set a variable, or \"NAME\" to unset it.
The default unsets LIBGL_ALWAYS_SOFTWARE, set by the Emacs snap, with
which mpv crashes when it shows a video."
  :type '(repeat string)
  :group 'r4)

(defcustom r4-player-video nil
  "When non-nil, play videos, else only audio.
They show in the player window, or pop out, see
`r4-player-video-pop-out'.  Toggle it with `r4-player-toggle-video'."
  :type 'boolean
  :group 'r4)

(defcustom r4-player-audio-quality "bestaudio/best"
  "The yt-dlp format of streamed audio."
  :type 'string
  :group 'r4)

(defcustom r4-player-video-quality "bestvideo[height<=?1080]+bestaudio/best"
  "The yt-dlp format of streamed videos."
  :type 'string
  :group 'r4)

(defcustom r4-player-prefer-downloads t
  "When non-nil, play downloaded files instead of streaming."
  :type 'boolean
  :group 'r4)

(defcustom r4-player-mode-line t
  "When non-nil, show the player in the mode line once it plays.
It turns `r4-player-mode-line-mode' on."
  :type 'boolean
  :group 'r4)

(defvar r4-player-queue nil
  "Vector of the `r4-track' in the play queue.")

(defvar r4-player-index nil
  "Index of the current track in `r4-player-queue'.")

(defvar r4-player-shuffle nil
  "When non-nil, play the queue in random order.")

(defvar r4-player-paused nil
  "Non-nil when the player is paused.")

(defvar r4-player-label nil
  "What the player plays, such as \"auto radio @slug\", or nil.
When non-nil, the queue plays in order, without shuffle.")

(defvar r4-player-queue-filter nil
  "Predicate of the tracks of the queue to play, or nil to play all.
Set by the filter of the queue, see `r4-queue-filter'.")

(defvar r4-player-next-function nil
  "Function choosing the next track, or nil to play the queue.
It returns a cons of the queue index and the start position in
seconds, or nil to play nothing.  Playing new tracks resets it.")

(defvar r4-player-time-pos nil
  "Position of the playing track in seconds, or nil.")

(defvar r4-player-video-aspect nil
  "Aspect ratio of the video playing, its width divided by its height, or nil.")

(defvar r4-player--duration nil
  "Duration of the playing track in seconds, given by mpv, or nil.")

(defvar r4-player-mode-line-extra nil
  "Text shown after r4 in the mode line, such as live while broadcasting.")

(defvar r4-player-track-changed-hook nil
  "Hook run after the current track changed.")

(defvar r4-player-changed-hook nil
  "Hook run after the player changed: its track, time, pause or options.")

(defvar r4-player-queue-changed-hook nil
  "Hook run after tracks were added to or removed from the queue.")

(defvar r4-player--history nil "Indexes of the previously played tracks.")
(defvar r4-player--start-at nil "Position in seconds to seek to, once loaded.")
(defvar r4-player--timer nil "Timer reading the position of the playing track.")

;;;; mpv

(defun r4-player-live-p ()
  "Return non-nil if the player is running."
  (r4-mpv-live-p))

(defun r4-player--send (&rest command)
  "Send COMMAND, a list of arguments, to mpv, starting it if needed."
  (unless (r4-mpv-live-p)
    (r4-mpv-start r4-player-program
                  (append (r4-video-mpv-arguments) r4-player-arguments)
                  r4-player-environment)
    (r4-mpv-send "observe_property" 1 "pause")
    (r4-mpv-send "observe_property" 2 "duration")
    (r4-mpv-send "observe_property" 3 "video-params/aspect")
    (r4-player--apply-video)
    (when r4-player-mode-line (r4-player-mode-line-mode 1))
    (r4-player--start-timer))
  (apply #'r4-mpv-send command))

(defun r4-player-get-property (name callback)
  "Call CALLBACK with the value of the mpv property NAME, or nil."
  (r4-mpv-get-property name callback))

(defun r4-player--apply-video ()
  "Show or hide videos, according to `r4-player-video'."
  (when (r4-mpv-live-p)
    (when r4-player-video
      (let ((window (r4-video-window-p)))
        (r4-mpv-send "set_property" "wid" (if window (r4-video-window-id) -1))
        ;; The keys go to Emacs, for the player keys.
        (r4-mpv-send "set_property" "input-vo-keyboard" (if window "no" "yes"))))
    (r4-mpv-send "set_property" "vid" (if r4-player-video "auto" "no"))
    (r4-mpv-send "set_property" "ytdl-format"
                 (if r4-player-video r4-player-video-quality r4-player-audio-quality)))
  (unless r4-player-video
    (r4-video-quit)))

(defun r4-player--video-window-quit ()
  "Turn videos off, as their window was quit."
  (setq r4-player-video nil)
  (r4-player--apply-video)
  (r4-player--changed)
  (message "r4: video off"))

(add-hook 'r4-video-window-quit-hook #'r4-player--video-window-quit)

(defun r4-player--changed ()
  "Run `r4-player-changed-hook' and update the mode lines."
  (run-hooks 'r4-player-changed-hook)
  (force-mode-line-update t))

(defun r4-player--start-timer ()
  "Read the position of the playing track every second."
  (unless (timerp r4-player--timer)
    (setq r4-player--timer (run-with-timer 1 1 #'r4-player--update-time))))

(defun r4-player--stop-timer ()
  "Stop reading the position of the playing track."
  (when (timerp r4-player--timer)
    (cancel-timer r4-player--timer))
  (setq r4-player--timer nil
        r4-player-time-pos nil
        r4-player--duration nil))

(defun r4-player--update-time ()
  "Read the position of the playing track, and show it when it changed."
  (when (and (r4-mpv-live-p) (not r4-player-paused))
    (r4-mpv-get-property
     "time-pos"
     (lambda (position)
       (unless (equal (and position (floor position))
                      (and r4-player-time-pos (floor r4-player-time-pos)))
         (setq r4-player-time-pos position)
         (r4-player--changed))))))

(defun r4-player-duration ()
  "Return the duration in seconds of the playing track, or nil.
It is the one given by mpv, or else the one of radio4000.com."
  (or r4-player--duration
      (when-let* ((track (r4-player-current))) (r4-track-duration track))))

(defun r4-player-time-string ()
  "Return the elapsed time and duration of the playing track, as 1:23/4:56."
  (let ((duration (r4-player-duration)))
    (concat (r4-format-duration (or r4-player-time-pos 0))
            (if duration (concat "/" (r4-format-duration duration)) ""))))

(defun r4-player--kill ()
  "Stop mpv."
  (r4-mpv-kill)
  (r4-video-quit)
  (r4-player--stop-timer)
  (setq r4-player-paused nil))

(defun r4-player--handle-event (event)
  "Handle the mpv EVENT, an alist."
  (let-alist event
    (pcase .event
      ("end-file"
       (pcase .reason
         ("eof" (r4-player-next))
         ("error"
          (message "r4: cannot play %s (%s)" (r4-player--current-title) .file_error)
          (r4-player-next))))
      ("file-loaded"
       (when r4-player--start-at
         (r4-mpv-send "seek" r4-player--start-at "absolute")
         (setq r4-player--start-at nil)))
      ("property-change"
       (pcase .name
         ("pause" (setq r4-player-paused .data))
         ("duration" (setq r4-player--duration .data))
         ("video-params/aspect" (setq r4-player-video-aspect .data)))
       (r4-player--changed)))))

(add-hook 'r4-mpv-event-functions #'r4-player--handle-event)
(add-hook 'r4-mpv-exit-hook (lambda ()
                              (r4-player--stop-timer)
                              (setq r4-player-paused nil)
                              (r4-player--changed)))

;;;; Queue

(defun r4-player-current ()
  "Return the `r4-track' currently playing, or nil."
  (and r4-player-index (aref r4-player-queue r4-player-index)))

(defun r4-player--current-title ()
  "Return the title of the current track."
  (if-let* ((track (r4-player-current))) (r4-track-title track) ""))

(defun r4-player--source (track)
  "Return the file or URL to play for TRACK."
  (or (and r4-player-prefer-downloads
           (r4-files-track-file track (if r4-player-video 'video 'audio)))
      (r4-track-url track)))

(defun r4-player--play-index (index &optional start)
  "Play the track at INDEX in the queue, from START seconds if non-nil."
  (let* ((track (aref r4-player-queue index))
         (source (r4-player--source track)))
    (setq r4-player-index index
          r4-player--start-at (and start (> start 0) start)
          r4-player-time-pos start
          r4-player--duration nil)
    (r4-player--send "loadfile" source "replace")
    (r4-player--send "set_property" "pause" :false)
    (message "r4: %s %s" (if (file-name-absolute-p source) "Playing" "Streaming")
             (r4-track-title track))
    (run-hooks 'r4-player-track-changed-hook)
    (r4-player--changed)))

(defun r4-player-play (tracks &optional index start label)
  "Replace the queue by the list TRACKS and play the one at INDEX.
If INDEX is nil, play the first track, or a random one when shuffling.
Start at START seconds if non-nil.  LABEL, see `r4-player-label',
plays the queue in order."
  (when (null tracks) (user-error "No tracks to play"))
  (setq r4-player-queue (vconcat tracks)
        r4-player--history nil
        r4-player-queue-filter nil
        r4-player-label label
        r4-player-next-function nil)
  (r4-player--play-index (or index
                             (if (and r4-player-shuffle (not label)) (random (length tracks)) 0))
                         start))

(defun r4-player--shuffling-p ()
  "Return non-nil if the next track is random."
  (and r4-player-shuffle (not r4-player-label)))

(defun r4-player--playable ()
  "Return the indexes of the queue tracks to play, passing its filter.
All of them when none passes `r4-player-queue-filter'."
  (let ((indexes (number-sequence 0 (1- (length r4-player-queue)))))
    (or (and r4-player-queue-filter
             (seq-filter (lambda (index)
                           (funcall r4-player-queue-filter (aref r4-player-queue index)))
                         indexes))
        indexes)))

(defun r4-player-next ()
  "Play the next track in the queue, among the tracks passing its filter."
  (interactive)
  (unless r4-player-index (user-error "Nothing is playing"))
  (push r4-player-index r4-player--history)
  (let ((indexes (r4-player--playable)))
    (cond (r4-player-next-function
           (when-let* ((next (funcall r4-player-next-function)))
             (r4-player--play-index (car next) (cdr next))))
          ((and (r4-player--shuffling-p) (cdr indexes))
           (let ((others (remq r4-player-index indexes)))
             (r4-player--play-index (nth (random (length others)) others))))
          (t (r4-player--play-index
              (or (seq-find (lambda (index) (> index r4-player-index)) indexes)
                  (car indexes)))))))

(defun r4-player-previous ()
  "Play the previously played track."
  (interactive)
  (unless r4-player-index (user-error "Nothing is playing"))
  (let ((indexes (r4-player--playable)))
    (r4-player--play-index
     (or (pop r4-player--history)
         (car (last (seq-filter (lambda (index) (< index r4-player-index)) indexes)))
         (car (last indexes))))))

(defun r4-player-toggle-pause ()
  "Pause or resume playback, or play the queue if stopped."
  (interactive)
  (cond ((r4-player-live-p) (r4-player--send "cycle" "pause"))
        ((> (length r4-player-queue) 0) (r4-player--play-index (or r4-player-index 0)))
        (t (user-error "Nothing to play"))))

(defun r4-player-toggle-video ()
  "Toggle showing videos, see `r4-player-video'.
The playing track plays again from the same position, from a source
with a video when turning it on, such as the stream instead of a
downloaded audio file."
  (interactive)
  (setq r4-player-video (not r4-player-video))
  (r4-player--apply-video)
  (message "r4: video %s" (if r4-player-video "on" "off"))
  (r4-player--changed)
  (r4-player--reload))

(defun r4-player-toggle-video-pop-out ()
  "Toggle showing videos in their own window, see `r4-player-video-pop-out'."
  (interactive)
  (setq r4-player-video-pop-out (not r4-player-video-pop-out))
  (message "r4: videos show in %s" (if r4-player-video-pop-out
                                       "their own window"
                                     "the player window"))
  (when r4-player-video
    (r4-video-quit)
    (r4-player--apply-video)
    (r4-player--changed)
    (r4-player--reload)))

(defun r4-player--reload ()
  "Play the playing track again from the same position."
  (when (and (r4-player-live-p) r4-player-index)
    (let ((index r4-player-index))
      (r4-player-get-property
       "time-pos"
       (lambda (position)
         (when (eql index r4-player-index)
           (r4-player--play-index index position)))))))

;;;; Queue operations

(defun r4-player-add-to-queue (tracks)
  "Add TRACKS at the end of the queue."
  (setq r4-player-queue (vconcat r4-player-queue tracks))
  (run-hooks 'r4-player-queue-changed-hook)
  (message "Added %d track%s to the queue" (length tracks) (if (cdr tracks) "s" "")))

(defun r4-player-play-next (tracks)
  "Insert TRACKS in the queue, after the current track."
  (let ((position (if r4-player-index (1+ r4-player-index) 0)))
    (setq r4-player-queue (vconcat (seq-take r4-player-queue position)
                                   tracks
                                   (seq-drop r4-player-queue position))))
  (run-hooks 'r4-player-queue-changed-hook)
  (message "%d track%s will play next" (length tracks) (if (cdr tracks) "s" "")))

(defun r4-player-remove (index)
  "Remove the track at INDEX from the queue."
  (when (eql index r4-player-index)
    (user-error "Cannot remove the playing track"))
  (setq r4-player-queue (vconcat (seq-take r4-player-queue index)
                                 (seq-drop r4-player-queue (1+ index)))
        r4-player--history nil)
  (when (and r4-player-index (> r4-player-index index))
    (cl-decf r4-player-index))
  (run-hooks 'r4-player-queue-changed-hook))

(defun r4-player-clear ()
  "Remove all the tracks of the queue, but the playing one."
  (interactive)
  (let ((current (r4-player-current)))
    (setq r4-player-queue (if current (vector current) [])
          r4-player-index (and current 0)
          r4-player--history nil))
  (run-hooks 'r4-player-queue-changed-hook)
  (message "Cleared the queue"))

(defun r4-player-toggle-shuffle ()
  "Toggle playing the queue in random order."
  (interactive)
  (setq r4-player-shuffle (not r4-player-shuffle))
  (message "r4: shuffle %s" (if r4-player-shuffle "on" "off"))
  (r4-player--changed))

(defun r4-player-seek (seconds)
  "Seek SECONDS forward, or backward if negative."
  (interactive "p")
  (unless (r4-player-live-p) (user-error "Nothing is playing"))
  (r4-player--send "seek" seconds "relative"))

(defun r4-player-seek-percent (percent)
  "Seek to PERCENT of the playing track."
  (unless (r4-player-live-p) (user-error "Nothing is playing"))
  (r4-player--send "seek" percent "absolute-percent"))

(defun r4-player-seek-forward ()
  "Seek 10 seconds forward."
  (interactive)
  (r4-player-seek 10))

(defun r4-player-seek-backward ()
  "Seek 10 seconds backward."
  (interactive)
  (r4-player-seek -10))

(defun r4-player-volume (delta)
  "Change the volume by DELTA percent."
  (unless (r4-player-live-p) (user-error "Nothing is playing"))
  (r4-player--send "add" "volume" delta))

(defun r4-player-volume-up ()
  "Raise the volume."
  (interactive)
  (r4-player-volume 5))

(defun r4-player-volume-down ()
  "Lower the volume."
  (interactive)
  (r4-player-volume -5))

(defun r4-player-stop ()
  "Stop playback and quit mpv."
  (interactive)
  (r4-player--kill)
  (setq r4-player-index nil
        r4-player-label nil
        r4-player-next-function nil)
  (run-hooks 'r4-player-track-changed-hook)
  (r4-player--changed)
  (message "r4: stopped"))

(defun r4-player--current-or-error ()
  "Return the current track, or signal an error if nothing is playing."
  (or (and r4-player-index (r4-player-current))
      (user-error "Nothing is playing")))

(defun r4-player-browse-current ()
  "Open the current track on radio4000.com."
  (interactive)
  (browse-url (r4-track-web-url (r4-player--current-or-error))))

(defun r4-player--copy (url)
  "Copy URL to the kill ring and say so."
  (kill-new url)
  (message "Copied %s" url))

(defun r4-player-copy-url ()
  "Copy the media URL of the current track, such as its YouTube link."
  (interactive)
  (r4-player--copy (r4-track-url (r4-player--current-or-error))))

(defun r4-player-copy-web-url ()
  "Copy the radio4000.com URL of the current track."
  (interactive)
  (r4-player--copy (r4-track-web-url (r4-player--current-or-error))))

;;;; Mode line

(defvar-keymap r4-player-mode-line-map
  :doc "Mouse keys of the player in the mode line."
  "<mode-line> <mouse-1>" #'r4-player-toggle-pause
  "<mode-line> <mouse-2>" #'r4-player-next
  "<mode-line> <mouse-3>" #'r4-queue)

(defun r4-player-mode-line-string ()
  "Return the mode line string of the player."
  (when-let* ((track (and (r4-player-live-p) (r4-player-current))))
    (propertize (format " %s%s %s %s%s %s "
                        (propertize "r4" 'face 'bold)
                        (if r4-player-mode-line-extra
                            (concat " " (propertize r4-player-mode-line-extra 'face 'error))
                          "")
                        (if r4-player-paused "⏸" "▶")
                        (if r4-player-label (format "[%s] " r4-player-label) "")
                        (truncate-string-to-width (r4-one-line (r4-track-title track)) 40 nil nil "…")
                        (r4-player-time-string))
                'help-echo (format "@%s: %s\nmouse-1: pause or resume\nmouse-2: next track\nmouse-3: queue"
                                   (r4-track-slug track) (r4-track-title track))
                'mouse-face 'mode-line-highlight
                'local-map r4-player-mode-line-map)))

(defvar r4-player--mode-line-construct '(:eval (r4-player-mode-line-string))
  "Mode line construct of the player.")
(put 'r4-player--mode-line-construct 'risky-local-variable t)

(define-minor-mode r4-player-mode-line-mode
  "Show the Radio4000 player in the mode line, while it plays.
It shows r4, ▶ or ⏸, what plays, and its elapsed time and duration.
Click it to pause, middle-click for the next track, right-click for
the queue."
  :global t
  :group 'r4
  (if r4-player-mode-line-mode
      (add-to-list 'global-mode-string 'r4-player--mode-line-construct t)
    (setq global-mode-string (delq 'r4-player--mode-line-construct global-mode-string)))
  (force-mode-line-update t))

;;;; Keys

(defvar-keymap r4-player-map
  :doc "Keys of the player, shared by the r4 buffers.
The r4 menus add the keys going to the main views, such as h home."
  "SPC" #'r4-player-toggle-pause
  ">" #'r4-player-next
  "<" #'r4-player-previous
  "]" #'r4-player-seek-forward
  "[" #'r4-player-seek-backward
  "+" #'r4-player-volume-up
  "-" #'r4-player-volume-down
  "Z" #'r4-player-toggle-shuffle
  "V" #'r4-player-toggle-video
  "^" #'r4-player-toggle-video-pop-out
  "k" #'r4-player-stop
  "Q" #'r4-queue)

(declare-function r4-queue "r4-queue")

(defun r4-player-list-map ()
  "Return a parent keymap for r4 lists: the player and tabulated list keys."
  (make-composed-keymap r4-player-map tabulated-list-mode-map))

(provide 'r4-player)

;;; r4-player.el ends here
