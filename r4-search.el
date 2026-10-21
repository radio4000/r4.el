;;; r4-search.el --- Search Radio4000 tracks  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The search language of radio4000.com: @slug targets a channel, #tag
;; a track tag, and other words search titles and descriptions.  They
;; combine freely, as in "@ko002 #jazz miles".
;;
;; `r4-search' searches all tracks, s in a channel searches its tracks,
;; and / and # filter the tracks shown in a list, without fetching.

;;; Code:

(require 'cl-lib)
(require 'r4-models)
(require 'r4-api)
(require 'r4-tracks)
(require 'r4-channels)
(require 'r4-filter)

(defvar r4-search-history nil
  "Minibuffer history of searches.")

;;;###autoload
(defun r4-search (text)
  "Search tracks with TEXT, such as \"@ko002 #jazz miles\".
@slug targets a channel, #tag a tag, and other words search the
titles and descriptions."
  (interactive (list (read-string "Search tracks (@channel #tag words): "
                                  nil 'r4-search-history)))
  (let ((source (r4-search-parse text)))
    (unless source (user-error "Empty search"))
    (r4-tracks-show (append (list :name (string-trim text)) source))))

(define-obsolete-function-alias 'r4-search-tracks #'r4-search "0.3.0")

(defun r4-tracks-search (text)
  "Search tracks with TEXT, in the channel of the buffer unless it has @slug.
In a local channel, filter its tracks instead, see `r4-tracks-filter'."
  (interactive (list (read-string "Search tracks (#tag words): "
                                  (and (plist-get r4-tracks--source :local)
                                       r4-tracks--filter-text)
                                  'r4-search-history)))
  (if (plist-get r4-tracks--source :local)
      (r4-tracks-filter text)
    (let ((slug (plist-get r4-tracks--source :slug))
          (source (r4-search-parse text)))
      (unless source (user-error "Empty search"))
      (when (and slug (not (plist-get source :slugs)))
        (setq text (format "@%s %s" slug text)
              source (append (list :slugs (list slug)) source)))
      (r4-tracks-show (append (list :name (string-trim text)) source)))))

;;;###autoload
(defun r4-mentions (slug)
  "List the tracks mentioning the channel SLUG, as @SLUG."
  (interactive (list (r4-read-channel "Tracks mentioning: @")))
  (r4-tracks-show (list :mention slug :name (format "mentions of @%s" slug))))

(defun r4-channels-mentions ()
  "List the tracks mentioning the channel at point."
  (interactive)
  (r4-mentions (r4-channel-slug (r4-channels--at-point))))

;;;; Filter the tracks shown

(defun r4-tracks-filter (text)
  "Only show the tracks matching TEXT, such as \"#jazz miles\".
An empty TEXT shows all the tracks."
  (interactive (list (read-string "Filter tracks (@channel #tag words): "
                                  r4-tracks--filter-text 'r4-search-history)))
  (r4-tracks-set-filter text)
  (message (if r4-tracks--filter-text
               (format "Showing %d of %d tracks matching \"%s\""
                       (length tabulated-list-entries) (length r4-tracks--tracks)
                       r4-tracks--filter-text)
             "Showing all tracks")))

(defun r4-tracks-filter-tag (tags)
  "Toggle filtering the shown tracks by TAGS, like the tags of radio4000.com.
Tags are chosen among those of the shown tracks, with their counts,
and the tags of the filter, to remove them.  Clicking a tag of a track
toggles it too."
  (interactive (list (r4-filter-read-tags (r4-tracks--visible) r4-tracks--filter-text)))
  (r4-tracks-filter (or (r4-filter-toggle-tags r4-tracks--filter-text tags) "")))

(defun r4-search--setup-tracks-filter ()
  "Make clicking a tag toggle it in the filter of the track list."
  (setq-local r4-filter-toggle-function #'r4-tracks-filter-tag))

(add-hook 'r4-tracks-mode-hook #'r4-search--setup-tracks-filter)

(defvar-local r4-search--channels-filter-text nil
  "Text of the filter of the channels shown.")

(defun r4-channels-filter (text)
  "Only show the channels whose slug, name or description match TEXT.
Every word of TEXT must match.  An empty TEXT shows all the channels."
  (interactive (list (read-string "Filter channels: " r4-search--channels-filter-text)))
  (let ((words (split-string (downcase text) "[ \t]+" t)))
    (setq r4-search--channels-filter-text (and words (string-trim text))
          r4-channels--filter
          (and words
               (lambda (channel)
                 (let ((haystack (downcase (concat "@" (r4-channel-slug channel) " "
                                                   (r4-channel-name channel) " "
                                                   (r4-channel-description channel)))))
                   (cl-every (lambda (word) (string-search word haystack)) words)))))
    (r4-channels-redisplay)))

(keymap-set r4-tracks-mode-map "s" #'r4-tracks-search)
(keymap-set r4-channels-mode-map "/" #'r4-channels-filter)
(keymap-set r4-tracks-mode-map "/" #'r4-tracks-filter)
(keymap-set r4-tracks-mode-map "#" #'r4-tracks-filter-tag)
(keymap-set r4-channels-mode-map "t" #'r4-search)
(keymap-set r4-channels-mode-map "M" #'r4-channels-mentions)

(provide 'r4-search)

;;; r4-search.el ends here
