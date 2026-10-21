;;; r4-broadcasting.el --- Broadcast your Radio4000 player  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Broadcast what you play as one of your channels, like the live
;; button of radio4000.com: listeners hear the same tracks, at the same
;; position, on radio4000.com or in Emacs (see r4-broadcast.el).
;;
;; Like radio4000.com, the broadcast is a row of the broadcast table,
;; read by listeners when they join, and "state" messages sent to them
;; through Supabase Realtime, when the track changes, pauses or seeks,
;; and every `r4-broadcast-send-interval' seconds.  Emacs sends them
;; with the HTTP API of Realtime, so it needs no websocket.

;;; Code:

(require 'cl-lib)
(require 'r4-models)
(require 'r4-api)
(require 'r4-auth)
(require 'r4-player)
(require 'r4-follows)
(require 'r4-broadcast)

(defcustom r4-broadcast-send-interval 10
  "Seconds between two states sent to the listeners of your broadcast."
  :type 'natnum
  :group 'r4)

(defconst r4-broadcasting--nil-uuid "00000000-0000-0000-0000-000000000000"
  "Track id of the broadcast table for tracks not on radio4000.com.")

(defvar r4-broadcasting nil
  "Your `r4-channel' broadcasting the player, or nil.")

(defvar r4-broadcasting--timer nil
  "Timer sending the state of the player to the listeners.")

(defvar r4-broadcasting--seq 0
  "Number of the last state sent, for listeners to ignore older ones.")

(defvar r4-broadcasting--sent nil
  "The last state sent: plist of :track-id, :playing, :position and :time.")

(defvar r4-broadcasting--track-started nil
  "Time the playing track started, as an ISO 8601 timestamp.")

(defun r4-broadcasting--now ()
  "Return the current time, as an ISO 8601 timestamp."
  (format-time-string "%FT%T.%3NZ" nil t))

(defun r4-broadcasting--track-id (track)
  "Return the broadcast id of TRACK, its id on radio4000.com."
  (if (r4-track-local-p track) (concat "local-" (r4-track-id track)) (r4-track-id track)))

(defun r4-broadcasting-deck (track position playing)
  "Return the deck state of TRACK at POSITION, PLAYING or not.
Like composeBroadcastDeckState of radio4000.com, without the volume,
which stays the listener's.  A track not on radio4000.com, such as
one of a local channel, comes with its URL and title."
  `((index . 0)
    (track_id . ,(r4-broadcasting--track-id track))
    (track_played_at . ,(or r4-broadcasting--track-started (r4-broadcasting--now)))
    (seeked_at . ,(r4-broadcasting--now))
    (seek_position . ,(or position 0))
    (is_playing . ,(if playing t :false))
    (speed . 1)
    ,@(when (r4-track-local-p track)
        `((track_url . ,(r4-track-url track))
          (track_title . ,(r4-track-title track))
          (track_media_id . ,(or (r4-track-media-id track) :null))))))

(defun r4-broadcasting--token ()
  "Return the access token of your session, without asking to sign in, or nil."
  (plist-get (ignore-errors (r4-auth-session)) :access-token))

(defun r4-broadcasting--send-deck (deck)
  "Send DECK to the listeners, and save it in the broadcast table."
  (if-let* ((token (r4-broadcasting--token)))
      (r4-broadcasting--send-deck-with deck token)
    (message "r4: your session ended, sign in with r4-login to broadcast again")
    (r4-broadcast-stop)))

(defun r4-broadcasting--send-deck-with (deck token)
  "Send DECK to the listeners, and save it in the broadcast table, with TOKEN."
  (let* ((channel-id (r4-channel-id r4-broadcasting))
         (track-id (alist-get 'track_id deck))
         (r4-api-error-function (lambda (error) (message "r4: broadcast: %s" error))))
    (r4-api-send "POST" "/realtime/v1/api/broadcast"
                 :token token
                 :body `((messages
                          . [((topic . ,(concat "broadcast-state:" channel-id))
                              (event . "state")
                              (private . :false)
                              (payload . ((channel_id . ,channel-id)
                                          (decks . [,deck])
                                          (seq . ,(cl-incf r4-broadcasting--seq)))))])))
    (r4-api-send "POST" "/rest/v1/broadcast"
                 :token token
                 :params '(("on_conflict" "channel_id"))
                 :prefer "resolution=merge-duplicates,return=minimal"
                 :body `((channel_id . ,channel-id)
                         (track_id . ,(if (string-prefix-p "local-" track-id)
                                          r4-broadcasting--nil-uuid
                                        track-id))
                         (track_played_at . ,(alist-get 'track_played_at deck))
                         (decks . [,deck])))))

(defun r4-broadcasting-send ()
  "Send the state of the player to the listeners of your broadcast."
  (when-let* ((track (and r4-broadcasting (r4-player-live-p) (r4-player-current))))
    (let ((position r4-player-time-pos)
          (playing (not r4-player-paused)))
      (setq r4-broadcasting--sent (list :track-id (r4-track-id track) :playing playing
                                        :position (or position 0) :time (float-time)))
      (r4-broadcasting--send-deck (r4-broadcasting-deck track position playing)))))

(defun r4-broadcasting--changed-p ()
  "Return non-nil if the player changed since the last state sent.
Another track, a pause, or a seek of more than `r4-broadcast-tolerance'."
  (let ((track (r4-player-current))
        (sent r4-broadcasting--sent))
    (or (null sent)
        (not (equal (and track (r4-track-id track)) (plist-get sent :track-id)))
        (not (eq (not r4-player-paused) (plist-get sent :playing)))
        (and r4-player-time-pos
             (> (abs (- r4-player-time-pos
                        (+ (plist-get sent :position)
                           (if (plist-get sent :playing)
                               (- (float-time) (plist-get sent :time))
                             0))))
                r4-broadcast-tolerance)))))

(defun r4-broadcasting--player-changed ()
  "Send the state of the player when it changed, while broadcasting."
  (when (and r4-broadcasting (r4-broadcasting--changed-p))
    (r4-broadcasting-send)))

(defun r4-broadcasting--track-changed ()
  "Note when the track started, or stop broadcasting when the player stops."
  (when r4-broadcasting
    (if (r4-player-current)
        (setq r4-broadcasting--track-started (r4-broadcasting--now))
      (r4-broadcast-stop))))

(add-hook 'r4-player-track-changed-hook #'r4-broadcasting--track-changed)
(add-hook 'r4-player-changed-hook #'r4-broadcasting--player-changed)

;;;###autoload
(defun r4-broadcast-start ()
  "Broadcast what you play as your channel, like the live button of radio4000.com."
  (interactive)
  (r4-auth-require)
  (unless (and (r4-player-live-p) (r4-player-current))
    (user-error "Play something first, then broadcast it"))
  (let ((channel (r4-follows-my-channel)))
    (r4-broadcast-leave)
    (when r4-broadcasting--timer (cancel-timer r4-broadcasting--timer))
    (setq r4-broadcasting channel
          r4-broadcasting--sent nil
          r4-broadcasting--track-started (r4-broadcasting--now)
          r4-broadcasting--timer (run-with-timer r4-broadcast-send-interval
                                                 r4-broadcast-send-interval
                                                 #'r4-broadcasting-send)
          r4-player-mode-line-extra "live")
    (r4-broadcasting-send)
    (force-mode-line-update t)
    (message "r4: broadcasting as @%s, listeners hear what you play"
             (r4-channel-slug channel))))

(defun r4-broadcast-stop ()
  "Stop broadcasting your player."
  (interactive)
  (when-let* ((channel r4-broadcasting))
    (when r4-broadcasting--timer (cancel-timer r4-broadcasting--timer))
    (setq r4-broadcasting nil
          r4-broadcasting--timer nil
          r4-broadcasting--sent nil
          r4-player-mode-line-extra nil)
    ;; Waiting for it, also when Emacs quits, for listeners to see it ended.
    (condition-case err
        (r4-api-request "DELETE" "/rest/v1/broadcast"
                        :token (or (r4-broadcasting--token) (error "Not signed in"))
                        :params `(("channel_id" ,(concat "eq." (r4-channel-id channel)))))
      (error (message "r4: broadcast: %s" (error-message-string err))))
    (force-mode-line-update t)
    (message "r4: stopped broadcasting as @%s" (r4-channel-slug channel))))

(defun r4-broadcast-toggle ()
  "Start broadcasting your player, or stop."
  (interactive)
  (if r4-broadcasting (r4-broadcast-stop) (r4-broadcast-start)))

(add-hook 'kill-emacs-hook #'r4-broadcast-stop)

(provide 'r4-broadcasting)

;;; r4-broadcasting.el ends here
