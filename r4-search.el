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

(defvar r4-search-history nil
  "Minibuffer history of searches.")

(defun r4-search-parse (text)
  "Parse the search TEXT into a plist of :slugs, :tags and :query."
  (let (slugs tags words)
    (dolist (token (split-string (or text "") "[ \t\n]+" t))
      (cond ((string-match "\\`@\\(.+\\)" token)
             (push (downcase (match-string 1 token)) slugs))
            ((string-match "\\`#\\(.+\\)" token)
             (push (downcase (match-string 1 token)) tags))
            (t (push token words))))
    `(,@(when slugs (list :slugs (nreverse slugs)))
      ,@(when tags (list :tags (nreverse tags)))
      ,@(when words (list :query (string-join (nreverse words) " "))))))

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

(defvar-local r4-search--filter-text nil
  "Text of the filter of the tracks shown.")

(defun r4-tracks-search (text)
  "Search tracks with TEXT, in the channel of the buffer unless it has @slug.
In a local channel, filter its tracks instead, see `r4-tracks-filter'."
  (interactive (list (read-string "Search tracks (#tag words): "
                                  (and (plist-get r4-tracks--source :local)
                                       r4-search--filter-text)
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

(defun r4-search-matcher (text)
  "Return a predicate of the tracks matching the search TEXT, or nil.
Every @slug, #tag and word must match."
  (let* ((parsed (r4-search-parse text))
         (slugs (plist-get parsed :slugs))
         (tags (plist-get parsed :tags))
         (words (split-string (downcase (or (plist-get parsed :query) "")) " " t)))
    (when parsed
      (lambda (track)
        (and (or (null slugs) (member (downcase (r4-track-slug track)) slugs))
             (cl-every (lambda (tag)
                         (member tag (mapcar #'downcase (r4-track-tags track))))
                       tags)
             (let ((haystack (downcase (concat (r4-track-title track) " "
                                               (r4-track-description track) " @"
                                               (r4-track-slug track)))))
               (cl-every (lambda (word) (string-search word haystack)) words)))))))

(defun r4-tracks-filter (text)
  "Only show the tracks matching TEXT, such as \"#jazz miles\".
An empty TEXT shows all the tracks."
  (interactive (list (read-string "Filter tracks (@channel #tag words): "
                                  r4-search--filter-text 'r4-search-history)))
  (setq r4-search--filter-text (if (string-blank-p text) nil (string-trim text)))
  (r4-tracks-set-filter (and r4-search--filter-text
                             (r4-search-matcher r4-search--filter-text)))
  (message (if r4-search--filter-text
               (format "Showing %d of %d tracks matching \"%s\""
                       (length tabulated-list-entries) (length r4-tracks--tracks)
                       r4-search--filter-text)
             "Showing all tracks")))

(defun r4-search-tag-counts (tracks)
  "Return an alist of the tags of TRACKS and their counts, most used first."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (track tracks)
      (dolist (tag (r4-track-tags track))
        (cl-incf (gethash (downcase tag) counts 0))))
    (sort (let (result)
            (maphash (lambda (tag count) (push (cons tag count) result)) counts)
            result)
          (lambda (a b) (> (cdr a) (cdr b))))))

(defun r4-tracks-filter-tag (tags)
  "Only show the tracks having all the TAGS, chosen among the shown tracks."
  (interactive
   (let* ((counts (r4-search-tag-counts (r4-tracks--visible)))
          (completion-extra-properties
           `(:annotation-function
             ,(lambda (tag) (format "  %d" (alist-get tag counts nil nil #'equal))))))
     (unless counts (user-error "No tags"))
     (list (completing-read-multiple
            "Tags: "
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  '(metadata (display-sort-function . identity) (category . r4-tag))
                (complete-with-action action (mapcar #'car counts) string predicate)))
            nil t))))
  (r4-tracks-filter (string-join (cons (or r4-search--filter-text "")
                                       (mapcar (lambda (tag) (concat "#" tag)) tags))
                                 " ")))

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
