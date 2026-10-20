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
(require 'r4-download)

(defcustom r4-player-program "mpv"
  "Program used to play tracks."
  :type 'string
  :group 'r4)

(defcustom r4-player-arguments '("--no-video" "--ytdl-format=bestaudio/best")
  "Extra arguments passed to `r4-player-program'."
  :type '(repeat string)
  :group 'r4)

(defcustom r4-player-prefer-downloads t
  "When non-nil, play downloaded files instead of streaming."
  :type 'boolean
  :group 'r4)

(defcustom r4-player-mode-line t
  "When non-nil, show the current track in the mode line."
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

(defvar r4-player-track-changed-hook nil
  "Hook run after the current track changed.")

(defvar r4-player--process nil "The mpv process.")
(defvar r4-player--ipc nil "The network process connected to mpv.")
(defvar r4-player--history nil "Indexes of the previously played tracks.")

(defun r4-player--socket ()
  "Return the file name of the mpv IPC socket."
  (expand-file-name (format "r4-mpv-%d.sock" (emacs-pid)) temporary-file-directory))

;;;; mpv process

(defun r4-player-live-p ()
  "Return non-nil if the player is running."
  (and (process-live-p r4-player--process) (process-live-p r4-player--ipc)))

(defun r4-player--start ()
  "Start mpv and connect to its IPC socket."
  (unless (executable-find r4-player-program)
    (user-error "Cannot find `%s', please install it" r4-player-program))
  (r4-player--kill)
  (let ((socket (r4-player--socket)))
    (ignore-errors (delete-file socket))
    (setq r4-player--process
          (make-process
           :name "r4-player"
           :buffer " *r4-player*"
           :command `(,r4-player-program "--idle=yes" "--no-terminal"
                                         ,(concat "--input-ipc-server=" socket)
                                         ,@r4-player-arguments)
           :noquery t
           :sentinel #'r4-player--sentinel))
    (with-timeout (5 (r4-player--kill) (error "Timeout waiting for mpv to start"))
      (while (not (file-exists-p socket))
        (accept-process-output r4-player--process 0.05)))
    (setq r4-player--ipc
          (make-network-process
           :name "r4-player-ipc"
           :family 'local
           :service socket
           :coding 'utf-8
           :noquery t
           :filter #'r4-player--filter))
    (r4-player--send "observe_property" 1 "pause")
    (r4-player--mode-line-setup)))

(defun r4-player--kill ()
  "Stop mpv and forget the connection."
  (dolist (process (list r4-player--ipc r4-player--process))
    (when (process-live-p process) (delete-process process)))
  (setq r4-player--ipc nil r4-player--process nil r4-player-paused nil))

(defun r4-player--sentinel (process _event)
  "Clean up when the mpv PROCESS exits."
  (unless (process-live-p process)
    (when (eq process r4-player--process)
      (r4-player--kill)
      (force-mode-line-update t))))

(defun r4-player--send (&rest command)
  "Send COMMAND, a list of arguments, to mpv."
  (unless (r4-player-live-p) (r4-player--start))
  (process-send-string r4-player--ipc
                       (concat (json-serialize `((command . ,(vconcat command)))) "\n")))

(defun r4-player--filter (process output)
  "Handle the JSON events in OUTPUT sent by mpv on PROCESS."
  (let ((lines (split-string (concat (process-get process 'r4-pending) output) "\n")))
    (process-put process 'r4-pending (car (last lines)))
    (dolist (line (butlast lines))
      (unless (string-empty-p line)
        (r4-player--handle-event
         (json-parse-string line :object-type 'alist :null-object nil :false-object nil))))))

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
      ("property-change"
       (when (equal .name "pause")
         (setq r4-player-paused .data)
         (force-mode-line-update t))))))

;;;; Queue

(defun r4-player-current ()
  "Return the `r4-track' currently playing, or nil."
  (and r4-player-index (aref r4-player-queue r4-player-index)))

(defun r4-player--current-title ()
  "Return the title of the current track."
  (if-let* ((track (r4-player-current))) (r4-track-title track) ""))

(defun r4-player--source (track)
  "Return the file or URL to play for TRACK."
  (or (and r4-player-prefer-downloads (r4-download-file track))
      (r4-track-url track)))

(defun r4-player--play-index (index)
  "Play the track at INDEX in the queue."
  (let* ((track (aref r4-player-queue index))
         (source (r4-player--source track)))
    (setq r4-player-index index)
    (r4-player--send "loadfile" source "replace")
    (r4-player--send "set_property" "pause" :false)
    (message "r4: %s %s" (if (file-name-absolute-p source) "Playing" "Streaming")
             (r4-track-title track))
    (run-hooks 'r4-player-track-changed-hook)
    (force-mode-line-update t)))

(defun r4-player-play (tracks &optional index)
  "Replace the queue by the list TRACKS and play the one at INDEX.
If INDEX is nil, play the first track, or a random one when shuffling."
  (when (null tracks) (user-error "No tracks to play"))
  (setq r4-player-queue (vconcat tracks)
        r4-player--history nil)
  (r4-player--play-index (or index
                             (if r4-player-shuffle (random (length tracks)) 0))))

(defun r4-player-next ()
  "Play the next track in the queue."
  (interactive)
  (unless r4-player-index (user-error "Nothing is playing"))
  (push r4-player-index r4-player--history)
  (let ((size (length r4-player-queue)))
    (r4-player--play-index (if (and r4-player-shuffle (> size 1))
                               (mod (+ r4-player-index 1 (random (1- size))) size)
                             (mod (1+ r4-player-index) size)))))

(defun r4-player-previous ()
  "Play the previously played track."
  (interactive)
  (unless r4-player-index (user-error "Nothing is playing"))
  (r4-player--play-index (or (pop r4-player--history)
                             (mod (1- r4-player-index) (length r4-player-queue)))))

(defun r4-player-toggle-pause ()
  "Pause or resume playback."
  (interactive)
  (unless (r4-player-live-p) (user-error "Nothing is playing"))
  (r4-player--send "cycle" "pause"))

(defun r4-player-toggle-shuffle ()
  "Toggle playing the queue in random order."
  (interactive)
  (setq r4-player-shuffle (not r4-player-shuffle))
  (message "r4: shuffle %s" (if r4-player-shuffle "on" "off")))

(defun r4-player-seek (seconds)
  "Seek SECONDS forward, or backward if negative."
  (interactive "p")
  (unless (r4-player-live-p) (user-error "Nothing is playing"))
  (r4-player--send "seek" seconds "relative"))

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
  (setq r4-player-index nil)
  (run-hooks 'r4-player-track-changed-hook)
  (force-mode-line-update t)
  (message "r4: stopped"))

(defun r4-player-browse-current ()
  "Open the current track on radio4000.com."
  (interactive)
  (browse-url (r4-track-web-url (or (r4-player-current) (user-error "Nothing is playing")))))

;;;; Mode line

(defun r4-player-mode-line-string ()
  "Return the mode line string of the player."
  (when-let* ((track (and r4-player-mode-line (r4-player-live-p) (r4-player-current))))
    (propertize (format " %s %s " (if r4-player-paused "⏸" "▶")
                        (truncate-string-to-width (r4-one-line (r4-track-title track)) 40 nil nil "…"))
                'help-echo (format "@%s: %s" (r4-track-slug track) (r4-track-title track)))))

(defvar r4-player--mode-line-construct '(:eval (r4-player-mode-line-string))
  "Mode line construct of the player.")
(put 'r4-player--mode-line-construct 'risky-local-variable t)

(defun r4-player--mode-line-setup ()
  "Add the player to `global-mode-string'."
  (add-to-list 'global-mode-string 'r4-player--mode-line-construct t))

;;;; Queue buffer

(defconst r4-queue-buffer "*r4 queue*"
  "Name of the buffer listing the play queue.")

(defun r4-queue--entries ()
  "Return the `tabulated-list-entries' of the play queue."
  (cl-loop for track across r4-player-queue
           for index from 0
           collect (list index
                         (vector (if (eql index r4-player-index) "▶" "")
                                 (concat "@" (r4-track-slug track))
                                 (r4-one-line (r4-track-title track))
                                 (r4-format-duration (r4-track-duration track))))))

(defun r4-queue-play ()
  "Play the queue track at point."
  (interactive)
  (r4-player--play-index (or (tabulated-list-get-id) (user-error "No track at point"))))

(defun r4-queue--refresh ()
  "Refresh the queue buffer, if it exists."
  (when-let* ((buffer (get-buffer r4-queue-buffer)))
    (with-current-buffer buffer (revert-buffer))))

(add-hook 'r4-player-track-changed-hook #'r4-queue--refresh)

(defvar-keymap r4-queue-mode-map
  :doc "Keymap for `r4-queue-mode'."
  "RET" #'r4-queue-play
  "SPC" #'r4-player-toggle-pause
  "n" #'r4-player-next
  "p" #'r4-player-previous
  "z" #'r4-player-toggle-shuffle
  "f" #'r4-player-seek-forward
  "b" #'r4-player-seek-backward
  "+" #'r4-player-volume-up
  "-" #'r4-player-volume-down
  "k" #'r4-player-stop)

(define-derived-mode r4-queue-mode tabulated-list-mode "r4 queue"
  "Major mode listing the Radio4000 play queue.

\\{r4-queue-mode-map}"
  (setq tabulated-list-format [(" " 1 nil) ("Channel" 20 t) ("Title" 60 t)
                               ("Time" 8 nil :right-align t)]
        tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook
            (lambda () (setq tabulated-list-entries (r4-queue--entries)))
            nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun r4-queue ()
  "Show the Radio4000 play queue."
  (interactive)
  (with-current-buffer (get-buffer-create r4-queue-buffer)
    (r4-queue-mode)
    (revert-buffer)
    (when r4-player-index
      (goto-char (point-min))
      (forward-line r4-player-index))
    (pop-to-buffer (current-buffer))))

(provide 'r4-player)

;;; r4-player.el ends here
