;;; r4-auto-radio.el --- Radio4000 auto-radio  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Auto-radio is a live radio without a broadcaster, like the infinity
;; button of radio4000.com: everyone listening to the same channel
;; hears the same track, at the same second.
;;
;; The tracks with a duration are shuffled the same way for everyone,
;; with a pseudo-random generator seeded by the week (it changes every
;; Sunday at 00:00 UTC), the date of the oldest track, and the channel.
;; The time elapsed since that oldest track, wrapped around the total
;; duration, gives the track and its position.  This is a port of
;; src/lib/player/auto-radio.ts of radio4000.com, with the same
;; results.

;;; Code:

(require 'cl-lib)
(require 'r4-models)
(require 'r4-api)
(require 'r4-player)
(require 'r4-tracks)
(require 'r4-channels)

(defconst r4-auto-radio-coverage 0.5
  "Minimum share of tracks with a duration to start an auto-radio.")

;;;; 32-bit arithmetic, like JavaScript

(defsubst r4-auto-radio--u32 (n)
  "Return N modulo 2^32."
  (logand n #xFFFFFFFF))

(defsubst r4-auto-radio--imul (a b)
  "Return the 32-bit product of A and B, like Math.imul."
  (logand (* a b) #xFFFFFFFF))

(defun r4-auto-radio--mulberry32 (seed)
  "Return a generator of 32-bit random integers, seeded with SEED."
  (let ((state (r4-auto-radio--u32 seed)))
    (lambda ()
      (setq state (r4-auto-radio--u32 (+ state #x6d2b79f5)))
      (let* ((z (r4-auto-radio--imul (logxor state (ash state -15)) (logior 1 state)))
             (z (r4-auto-radio--u32
                 (+ z (r4-auto-radio--imul (logxor z (ash z -7)) (logior 61 z))))))
        (logxor z (ash z -14))))))

(defun r4-auto-radio--hash-seed (n)
  "Mix the integer N, to seed the generator."
  (let ((h (r4-auto-radio--u32 n)))
    (setq h (r4-auto-radio--imul (logxor h (ash h -16)) #x45d9f3b)
          h (r4-auto-radio--imul (logxor h (ash h -16)) #x45d9f3b))
    (logxor h (ash h -16))))

(defun r4-auto-radio--hash-string (string)
  "Return the DJB2 hash of the UTF-16 code units of STRING."
  (let ((h 5381)
        (bytes (encode-coding-string string 'utf-16be)))
    (cl-loop for i from 0 below (length bytes) by 2
             for unit = (+ (ash (aref bytes i) 8) (aref bytes (1+ i)))
             do (setq h (r4-auto-radio--u32 (+ (ash h 5) h unit))))
    h))

(defun r4-auto-radio--week (now-ms)
  "Return the number of Sunday-aligned weeks since 1970 at NOW-MS."
  (floor (+ now-ms (* 4 24 60 60 1000)) (* 7 24 60 60 1000)))

(defun r4-auto-radio--shuffle (items random)
  "Return a copy of the list ITEMS shuffled with the generator RANDOM."
  (let* ((array (vconcat items))
         (m (length array)))
    (while (> m 0)
      (let ((i (ash (* (funcall random) m) -32)))
        (cl-decf m)
        (cl-rotatef (aref array m) (aref array i))))
    (append array nil)))

;;;; Schedule

(defun r4-auto-radio-tracks (tracks)
  "Return the TRACKS that can play in an auto-radio: with a duration and URL."
  (cl-remove-if-not (lambda (track)
                      (and (numberp (r4-track-duration track))
                           (> (r4-track-duration track) 0)
                           (r4-track-url track)))
                    tracks))

(defun r4-auto-radio-epoch (tracks)
  "Return the creation of the oldest of TRACKS, in Unix seconds."
  (if tracks
      (floor (float-time
              (date-to-time (car (sort (mapcar #'r4-track-created-at tracks) #'string<)))))
    0))

(defun r4-auto-radio-schedule (tracks epoch now-ms &optional seed)
  "Return the shuffled TRACKS of the week of NOW-MS, from EPOCH.
SEED, a string such as \"@slug\", differentiates the shuffle of
different track lists."
  (let* ((sorted (sort (copy-sequence (r4-auto-radio-tracks tracks))
                       (lambda (a b) (string< (r4-track-id a) (r4-track-id b)))))
         (raw (+ epoch (r4-auto-radio--week now-ms)
                 (if seed (r4-auto-radio--hash-string seed) 0))))
    (r4-auto-radio--shuffle
     sorted (r4-auto-radio--mulberry32 (r4-auto-radio--hash-seed raw)))))

(defun r4-auto-radio-position (schedule epoch now-ms)
  "Return the playing index of SCHEDULE at NOW-MS and its offset in seconds.
The schedule started at EPOCH, in Unix seconds, and loops.  The result
is a cons (INDEX . OFFSET), or nil if SCHEDULE is empty."
  (let ((total (apply #'+ (mapcar #'r4-track-duration schedule))))
    (when (> total 0)
      (let ((position (mod (- (/ now-ms 1000.0) epoch) total))
            (start 0)
            (index 0))
        (cl-loop for track in schedule
                 for i from 0
                 do (setq index i)
                 until (< position (+ start (r4-track-duration track)))
                 do (cl-incf start (r4-track-duration track)))
        (cons index (- position start))))))

;;;; Playing

(defvar r4-auto-radio--state nil
  "Plist of the playing auto-radio: :tracks, :seed and :label.")

(defun r4-auto-radio--now-ms ()
  "Return the current time in milliseconds."
  (floor (* 1000 (float-time))))

(defun r4-auto-radio--next ()
  "Return the next track of the auto-radio, as (INDEX . OFFSET).
When the current track ended before its scheduled end, play the next
one from its start."
  (let* ((tracks (plist-get r4-auto-radio--state :tracks))
         (epoch (r4-auto-radio-epoch tracks))
         (now (r4-auto-radio--now-ms))
         (schedule (r4-auto-radio-schedule tracks epoch now (plist-get r4-auto-radio--state :seed)))
         (position (r4-auto-radio-position schedule epoch now)))
    (setq r4-player-queue (vconcat schedule))
    (if (eql (car position) r4-player-index)
        (cons (mod (1+ (car position)) (length schedule)) 0)
      position)))

(defun r4-auto-radio-play (tracks seed label)
  "Play TRACKS as an auto-radio, shuffled with SEED, named LABEL."
  (let* ((playable (r4-auto-radio-tracks tracks))
         (epoch (r4-auto-radio-epoch playable))
         (now (r4-auto-radio--now-ms)))
    (when (or (null playable)
              (< (/ (float (length playable)) (length tracks)) r4-auto-radio-coverage))
      (user-error "Not enough tracks with a duration for an auto-radio (%d of %d)"
                  (length playable) (length tracks)))
    (let* ((schedule (r4-auto-radio-schedule playable epoch now seed))
           (position (r4-auto-radio-position schedule epoch now)))
      (r4-player-play schedule (car position) (cdr position) label)
      (setq r4-auto-radio--state (list :tracks playable :seed seed :label label)
            r4-player-next-function #'r4-auto-radio--next)
      (message "r4: %s, %s at %s" label (r4-track-title (nth (car position) schedule))
               (r4-format-duration (cdr position))))))

;;;###autoload
(defun r4-auto-radio (slug)
  "Tune in to the auto-radio of the channel SLUG, like radio4000.com.
Everyone listening hears the same track at the same second."
  (interactive (list (r4-read-channel "Auto-radio of channel: @")))
  (message "Fetching @%s…" slug)
  (r4-api-tracks (lambda (tracks)
                   (r4-auto-radio-play tracks (concat "@" slug) (concat "auto-radio @" slug)))
                 :slug slug))

(defun r4-tracks-auto-radio ()
  "Tune in to the auto-radio of the tracks shown.
For a whole channel, it is the same as on radio4000.com."
  (interactive)
  (let* ((source r4-tracks--source)
         (whole-channel (and (plist-get source :slug)
                             (not (plist-get source :query))
                             (not (plist-get source :local))
                             (null r4-tracks--filter)))
         (name (r4-tracks--describe-source source)))
    (r4-auto-radio-play (r4-tracks--visible)
                        (if whole-channel (concat "@" (plist-get source :slug)) name)
                        (concat "auto-radio " name))))

(defun r4-channels-auto-radio ()
  "Tune in to the auto-radio of the channel at point."
  (interactive)
  (r4-auto-radio (r4-channel-slug (r4-channels--at-point))))

(keymap-set r4-tracks-mode-map "R" #'r4-tracks-auto-radio)
(keymap-set r4-channels-mode-map "R" #'r4-channels-auto-radio)

(provide 'r4-auto-radio)

;;; r4-auto-radio.el ends here
