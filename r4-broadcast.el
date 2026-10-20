;;; r4-broadcast.el --- Listen to Radio4000 broadcasts  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A channel broadcasts its player live on radio4000.com: listeners
;; hear the same track, at the same position.  `r4-broadcasts' lists
;; the live broadcasts, RET listens to one: the player follows the
;; broadcaster, checking the broadcast every `r4-broadcast-interval'
;; seconds, until you play something else or `r4-broadcast-leave'.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'r4-models)
(require 'r4-api)
(require 'r4-player)
(require 'r4-channels)

(defcustom r4-broadcast-interval 5
  "Seconds between two checks of the broadcast you listen to."
  :type 'natnum
  :group 'r4)

(defcustom r4-broadcast-tolerance 2
  "Seconds the player may drift from the broadcast before seeking."
  :type 'natnum
  :group 'r4)

(defconst r4-broadcast--columns
  "channel_id,track_id,track_played_at,updated_at,decks,channels(slug,name)"
  "Columns selected from the `broadcast' table.")

;;;; The broadcast state

(defun r4-broadcast-deck (row)
  "Return the deck state of the broadcast ROW, an alist.
It is the first deck of the broadcaster, or the row itself."
  (or (car (alist-get 'decks row)) row))

(defun r4-broadcast-position (deck duration &optional now)
  "Return the position in seconds of the broadcast DECK at NOW, or nil.
DURATION is the duration of the track, if known.  Like
calculateSeekTime of radio4000.com."
  (let-alist deck
    (let* ((now (or now (float-time)))
           (speed (or .speed 1))
           (since (lambda (time) (- now (float-time (date-to-time time)))))
           (position
            (cond ((and .seek_position .seeked_at)
                   (let ((elapsed (funcall since .seeked_at)))
                     (and (>= elapsed 0)
                          (if .is_playing (+ .seek_position (* elapsed speed)) .seek_position))))
                  (.seek_position .seek_position)
                  (.track_played_at
                   (let ((elapsed (funcall since .track_played_at)))
                     (and (>= elapsed 0) (* elapsed speed)))))))
      (and position
           (not (and duration (>= position duration)))
           (round position)))))

;;;; Listening

(defvar r4-broadcast--listening nil
  "Plist of the broadcast listened to: :channel-id, :slug and :label.")

(defvar r4-broadcast--timer nil
  "Timer checking the broadcast listened to.")

(defun r4-broadcast--fetch (channel-id callback)
  "Call CALLBACK with the broadcast row of CHANNEL-ID, or nil."
  (r4-api-get "broadcast" `(("select" ,r4-broadcast--columns)
                            ("channel_id" ,(concat "eq." channel-id)))
              (lambda (rows) (funcall callback (car rows)))))

(defun r4-broadcast--fetch-track (id callback)
  "Call CALLBACK with the track ID."
  (r4-api-get "channel_tracks" `(("select" ,r4-api--track-columns) ("id" ,(concat "eq." id)))
              (lambda (rows) (funcall callback (and rows (r4-track-from-json (car rows)))))))

(defun r4-broadcast-leave ()
  "Stop listening to the broadcast."
  (interactive)
  (when r4-broadcast--timer (cancel-timer r4-broadcast--timer))
  (when r4-broadcast--listening
    (message "r4: left the broadcast of @%s" (plist-get r4-broadcast--listening :slug)))
  (setq r4-broadcast--timer nil
        r4-broadcast--listening nil))

(defun r4-broadcast--sync (row)
  "Make the player follow the broadcast ROW."
  (let* ((deck (r4-broadcast-deck row))
         (track-id (alist-get 'track_id deck))
         (current (r4-player-current))
         (label (plist-get r4-broadcast--listening :label)))
    (cond
     ((null track-id))
     ((not (and current (equal (r4-track-id current) track-id) (r4-player-live-p)))
      (r4-broadcast--fetch-track
       track-id
       (lambda (track)
         (when (and track r4-broadcast--listening)
           (r4-player-play (list track) 0
                           (r4-broadcast-position deck (r4-track-duration track))
                           label)
           (setq r4-player-next-function #'r4-broadcast--next)
           (plist-put r4-broadcast--listening :started t)))))
     ((not (alist-get 'is_playing deck))
      (unless r4-player-paused (r4-player-toggle-pause)))
     (t
      (when r4-player-paused (r4-player-toggle-pause))
      (when-let* ((expected (r4-broadcast-position deck (r4-track-duration current))))
        (r4-player-get-property
         "time-pos"
         (lambda (position)
           (when (and (numberp position) (> (abs (- position expected)) r4-broadcast-tolerance))
             (r4-player--send "seek" expected "absolute")))))))))

(defun r4-broadcast--check ()
  "Follow the broadcast listened to, or leave it if it ended or you left."
  (let ((listening r4-broadcast--listening))
    (cond
     ((null listening) (r4-broadcast-leave))
     ((and (plist-get listening :started)
           (not (equal r4-player-label (plist-get listening :label))))
      ;; You played something else.
      (r4-broadcast-leave))
     (t (r4-broadcast--fetch
         (plist-get listening :channel-id)
         (lambda (row)
           (when (eq listening r4-broadcast--listening)
             (if row
                 (r4-broadcast--sync row)
               (message "r4: the broadcast of @%s ended" (plist-get listening :slug))
               (r4-broadcast-leave)))))))))

(defun r4-broadcast--next ()
  "At the end of a track, check the broadcast for the next one."
  (run-with-timer 0 nil #'r4-broadcast--check)
  nil)

(defun r4-broadcast-listen (channel-id slug)
  "Listen to the broadcast of the channel CHANNEL-ID, named SLUG."
  (r4-broadcast-leave)
  (setq r4-broadcast--listening (list :channel-id channel-id :slug slug
                                      :label (concat "broadcast @" slug) :started nil)
        r4-broadcast--timer (run-with-timer 0 r4-broadcast-interval #'r4-broadcast--check))
  (message "r4: listening to the broadcast of @%s" slug))

;;;; List of broadcasts

(defconst r4-broadcasts-buffer "*r4 broadcasts*"
  "Name of the buffer listing the live broadcasts.")

(defun r4-broadcasts--entries (rows)
  "Return the `tabulated-list-entries' of the broadcast ROWS."
  (mapcar (lambda (row)
            (let-alist row
              (let ((deck (r4-broadcast-deck row)))
                (list row
                      (vector (concat "@" (or .channels.slug ""))
                              (r4-one-line .channels.name)
                              (if (alist-get 'is_playing deck t) "playing" "paused")
                              (r4-one-line (alist-get 'track_title deck))
                              (if .updated_at
                                  (format-time-string "%F %R" (date-to-time .updated_at))
                                ""))))))
          rows))

(defun r4-broadcasts-revert (&rest _)
  "Fetch the live broadcasts again."
  (let ((buffer (current-buffer)))
    (r4-api-get "broadcast" `(("select" ,r4-broadcast--columns) ("order" "updated_at.desc"))
                (lambda (rows)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (setq tabulated-list-entries (r4-broadcasts--entries rows))
                      (tabulated-list-print t)
                      (message "%d live broadcast%s" (length rows) (if (cdr rows) "s" ""))))))))

(defun r4-broadcasts-listen ()
  "Listen to the broadcast at point."
  (interactive)
  (let ((row (or (tabulated-list-get-id) (user-error "No broadcast at point"))))
    (r4-broadcast-listen (alist-get 'channel_id row)
                         (alist-get 'slug (alist-get 'channels row)))))

(defvar-keymap r4-broadcasts-mode-map
  :doc "Keymap for `r4-broadcasts-mode'."
  :parent (r4-player-list-map)
  "RET" #'r4-broadcasts-listen
  "l" #'r4-broadcast-leave)

(define-derived-mode r4-broadcasts-mode tabulated-list-mode "r4 broadcasts"
  "Major mode listing the live Radio4000 broadcasts.

\\{r4-broadcasts-mode-map}"
  (setq tabulated-list-format [("Channel" 20 t) ("Name" 24 t) ("State" 8 t)
                               ("Track" 40 t) ("Updated" 16 t)]
        tabulated-list-padding 2)
  (setq-local revert-buffer-function #'r4-broadcasts-revert)
  (tabulated-list-init-header))

;;;###autoload
(defun r4-broadcasts ()
  "List the live broadcasts of radio4000.com, RET listens to one."
  (interactive)
  (with-current-buffer (get-buffer-create r4-broadcasts-buffer)
    (r4-broadcasts-mode)
    (revert-buffer)
    (pop-to-buffer-same-window (current-buffer))))

(provide 'r4-broadcast)

;;; r4-broadcast.el ends here
