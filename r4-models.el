;;; r4-models.el --- Radio4000 data models  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Plain data structures for Radio4000 channels and tracks, built from
;; the JSON rows of the `channels_with_tracks' and `channel_tracks'
;; Supabase views.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup r4 nil
  "Browse, play and download Radio4000 channels."
  :group 'multimedia
  :prefix "r4-"
  :link '(url-link "https://radio4000.com"))

(defcustom r4-web-url "https://radio4000.com"
  "Base URL of the Radio4000 web app."
  :type 'string
  :group 'r4)

(defcustom r4-image-url "https://res.cloudinary.com/radio4000"
  "Base URL of the Radio4000 images (Cloudinary)."
  :type 'string
  :group 'r4)

(cl-defstruct (r4-channel (:constructor r4-channel-create)
                          (:copier nil))
  "A Radio4000 channel."
  id slug name description url image track-count latest-track-at
  created-at updated-at latitude longitude followers)

(cl-defstruct (r4-track (:constructor r4-track-create)
                        (:copier nil))
  "A Radio4000 track, belonging to the channel SLUG."
  id slug title description url tags duration provider media-id
  discogs-url created-at updated-at)

(defun r4-channel-from-json (row)
  "Return an `r4-channel' from the alist ROW."
  (let-alist row
    (r4-channel-create
     :id .id :slug .slug :name .name :description .description
     :url .url :image .image :track-count .track_count
     :latest-track-at .latest_track_at
     :created-at .created_at :updated-at .updated_at
     :latitude .latitude :longitude .longitude :followers .followers)))

(defun r4-track-from-json (row)
  "Return an `r4-track' from the alist ROW."
  (let-alist row
    (r4-track-create
     :id .id :slug .slug :title .title :description .description
     :url .url :tags .tags :duration .duration :provider .provider
     :media-id .media_id :discogs-url .discogs_url
     :created-at .created_at :updated-at .updated_at)))

(defun r4-channel-web-url (channel)
  "Return the radio4000.com URL of CHANNEL."
  (format "%s/%s" r4-web-url (r4-channel-slug channel)))

(defun r4-channel-rss-url (channel)
  "Return the RSS feed URL of CHANNEL."
  (format "%s/%s.rss" r4-web-url (r4-channel-slug channel)))

(defun r4-channel-map-url (channel)
  "Return the URL of a map at the coordinates of CHANNEL, or nil."
  (when (and (numberp (r4-channel-latitude channel)) (numberp (r4-channel-longitude channel)))
    (format "https://www.openstreetmap.org/?mlat=%s&mlon=%s#map=6/%s/%s"
            (r4-channel-latitude channel) (r4-channel-longitude channel)
            (r4-channel-latitude channel) (r4-channel-longitude channel))))

(defvar r4-track--local (make-hash-table :test #'eq :weakness 'key)
  "Tracks read from local channels, see r4-local.el.")

(defun r4-track-mark-local (track)
  "Mark TRACK as a track of a local channel, and return it."
  (puthash track t r4-track--local)
  track)

(defun r4-track-local-p (track)
  "Return non-nil if TRACK is a track of a local channel.
Local tracks are not on radio4000.com."
  (gethash track r4-track--local))

(defun r4-track-web-url (track)
  "Return the radio4000.com URL of TRACK.
Signal an error for a track of a local channel, not on radio4000.com."
  (when (r4-track-local-p track)
    (user-error "\"%s\" is in a local channel, not on radio4000.com"
                (r4-track-title track)))
  (format "%s/%s/tracks/%s" r4-web-url (r4-track-slug track) (r4-track-id track)))

(defun r4-channel-image-url (channel &optional size)
  "Return the URL of the image of CHANNEL, SIZE pixels square, or nil."
  (when-let* ((image (r4-channel-image channel)))
    (unless (string-empty-p image)
      (format "%s/image/upload/w_%d,h_%d,c_thumb,q_80/%s.jpg"
              r4-image-url (or size 250) (or size 250) image))))

(defun r4-format-duration (seconds)
  "Format SECONDS as M:SS or H:MM:SS, or an empty string if nil."
  (if (numberp seconds)
      (let ((seconds (floor seconds)))
        (if (>= seconds 3600)
            (format "%d:%02d:%02d" (/ seconds 3600) (% (/ seconds 60) 60) (% seconds 60))
          (format "%d:%02d" (/ seconds 60) (% seconds 60))))
    ""))

(defun r4-format-date (timestamp)
  "Return the YYYY-MM-DD part of the ISO TIMESTAMP, or an empty string."
  (if (and (stringp timestamp) (>= (length timestamp) 10))
      (substring timestamp 0 10)
    ""))

(defun r4-one-line (text)
  "Return TEXT on a single line, or an empty string if nil."
  (if (stringp text)
      (string-trim (replace-regexp-in-string "[\n\r\t]+" " " text))
    ""))

(provide 'r4-models)

;;; r4-models.el ends here
