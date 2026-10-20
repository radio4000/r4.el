;;; r4-explore.el --- Explore Radio4000 channels  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The filters of the channel explorer of radio4000.com: featured
;; channels, channels with an image, with many tracks, or broadcasting.

;;; Code:

(require 'cl-lib)
(require 'r4-models)
(require 'r4-api)
(require 'r4-channels)
(require 'r4-broadcast)

(defcustom r4-explore-featured-size 40
  "Number of featured channels."
  :type 'natnum
  :group 'r4)

(defun r4-explore-featured-score (channel)
  "Return the featured score of CHANNEL, like radio4000.com.
Followers count most, then the tracks, then the recent activity."
  (let* ((latest (r4-channel-latest-track-at channel))
         (days (and latest (/ (- (float-time) (float-time (date-to-time latest))) 86400))))
    (+ (* 3 (length (r4-channel-followers channel)))
       (* 2 (log (1+ (or (r4-channel-track-count channel) 0))))
       (cond ((null days) 0) ((<= days 30) 3) ((<= days 90) 2) ((<= days 180) 1) (t 0)))))

(defun r4-explore--featured (callback)
  "Call CALLBACK with the featured channels: the best scored, active this month."
  (r4-api-get "channels_with_tracks"
              `(("select" ,(concat r4-api--channel-columns ",followers"))
                ("latest_track_at" ,(concat "gte." (format-time-string
                                                   "%FT%TZ" (- (float-time) (* 30 86400)) t)))
                ("track_count" "gte.1"))
              (lambda (rows)
                (funcall callback
                         (seq-take (sort (mapcar #'r4-channel-from-json rows)
                                         (lambda (a b) (> (r4-explore-featured-score a)
                                                          (r4-explore-featured-score b))))
                                   r4-explore-featured-size)))))

(defun r4-explore--filtered (predicate)
  "Return a function fetching all the channels passing PREDICATE."
  (lambda (callback)
    (r4-api-channels (lambda (channels) (funcall callback (seq-filter predicate channels))))))

(defun r4-explore--more-than (count)
  "Return a predicate of the channels with more than COUNT tracks."
  (lambda (channel) (> (or (r4-channel-track-count channel) 0) count)))

(defconst r4-explore-filters
  `(("featured" . r4-explore--featured)
    ("all")
    ("with-artwork" . ,(r4-explore--filtered
                        (lambda (channel)
                          (not (string-empty-p (or (r4-channel-image channel) ""))))))
    ("with-more-than-10-tracks" . ,(r4-explore--filtered (r4-explore--more-than 10)))
    ("with-more-than-100-tracks" . ,(r4-explore--filtered (r4-explore--more-than 100)))
    ("with-more-than-1000-tracks" . ,(r4-explore--filtered (r4-explore--more-than 1000)))
    ("broadcasting"))
  "Filters of `r4-explore', and the functions fetching their channels.")

;;;###autoload
(defun r4-explore (filter)
  "Explore the channels of radio4000.com, with FILTER."
  (interactive (list (completing-read "Explore channels: " r4-explore-filters nil t nil nil
                                      "featured")))
  (pcase filter
    ("all" (r4-channels))
    ("broadcasting" (r4-broadcasts))
    (_ (r4-channels-show (concat filter " channels")
                         (or (cdr (assoc filter r4-explore-filters))
                             (user-error "Unknown filter %s" filter))
                         (list 'r4-explore filter)))))

(provide 'r4-explore)

;;; r4-explore.el ends here
