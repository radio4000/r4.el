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

(cl-defstruct (r4-channel (:constructor r4-channel-create)
                          (:copier nil))
  "A Radio4000 channel."
  id slug name description url image track-count latest-track-at
  created-at updated-at)

(cl-defstruct (r4-track (:constructor r4-track-create)
                        (:copier nil))
  "A Radio4000 track, belonging to the channel SLUG."
  id slug title description url tags duration provider media-id
  discogs-url created-at)

(defun r4-channel-from-json (row)
  "Return an `r4-channel' from the alist ROW."
  (let-alist row
    (r4-channel-create
     :id .id :slug .slug :name .name :description .description
     :url .url :image .image :track-count .track_count
     :latest-track-at .latest_track_at
     :created-at .created_at :updated-at .updated_at)))

(defun r4-track-from-json (row)
  "Return an `r4-track' from the alist ROW."
  (let-alist row
    (r4-track-create
     :id .id :slug .slug :title .title :description .description
     :url .url :tags .tags :duration .duration :provider .provider
     :media-id .media_id :discogs-url .discogs_url
     :created-at .created_at)))

(defun r4-channel-web-url (channel)
  "Return the radio4000.com URL of CHANNEL."
  (format "%s/%s" r4-web-url (r4-channel-slug channel)))

(defun r4-track-web-url (track)
  "Return the radio4000.com URL of TRACK."
  (format "%s/%s/tracks/%s" r4-web-url (r4-track-slug track) (r4-track-id track)))

(defun r4-format-duration (seconds)
  "Format SECONDS as M:SS or H:MM:SS, or an empty string if nil."
  (cond ((not (numberp seconds)) "")
        ((>= seconds 3600) (format-seconds "%h:%.2m:%.2s" seconds))
        (t (format-seconds "%m:%.2s" seconds))))

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
