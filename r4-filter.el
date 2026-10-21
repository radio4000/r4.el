;;; r4-filter.el --- Filter Radio4000 tracks by text and tags  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Filters of the tracks shown in a list or played from the queue, in
;; the search language of radio4000.com: @slug targets a channel, #tag
;; a track tag, and other words search titles and descriptions.  Tags
;; toggle, like on radio4000.com: click a tag of a track to filter by
;; it, click it in the mode line to remove it.

;;; Code:

(require 'cl-lib)
(require 'r4-models)
(require 'r4-faces)

(defvar-local r4-filter-toggle-function nil
  "Function toggling tags in the filter of the buffer, called with a list.")

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

(defun r4-filter-matcher (text)
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

(defun r4-filter-toggle-tags (text tags)
  "Return the filter TEXT with TAGS added, or removed when already in it.
Return nil when the filter is empty."
  (let ((tokens (split-string (or text "") "[ \t\n]+" t)))
    (dolist (tag tags)
      (let ((token (concat "#" (downcase tag))))
        (setq tokens (if (member token tokens)
                         (delete token tokens)
                       (append tokens (list token))))))
    (and tokens (string-join tokens " "))))

(defun r4-filter-tag-counts (tracks)
  "Return an alist of the tags of TRACKS and their counts, most used first."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (track tracks)
      (dolist (tag (r4-track-tags track))
        (cl-incf (gethash (downcase tag) counts 0))))
    (sort (let (result)
            (maphash (lambda (tag count) (push (cons tag count) result)) counts)
            result)
          (lambda (a b) (> (cdr a) (cdr b))))))

(defun r4-filter-read-tags (tracks text)
  "Read tags to toggle in the filter TEXT, among the tags of TRACKS.
The tags of TEXT come first, choosing them removes them."
  (let* ((selected (plist-get (r4-search-parse text) :tags))
         (counts (r4-filter-tag-counts tracks))
         (tags (append selected (cl-remove-if (lambda (tag) (member tag selected))
                                              (mapcar #'car counts))))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (tag)
               (if (member tag selected)
                   "  (remove)"
                 (format "  %d" (alist-get tag counts 0 nil #'equal)))))))
    (unless tags (user-error "No tags"))
    (completing-read-multiple
     "Toggle tags: "
     (lambda (string predicate action)
       (if (eq action 'metadata)
           '(metadata (display-sort-function . identity) (category . r4-tag))
         (complete-with-action action tags string predicate)))
     nil t)))

;;;; Clickable tags

(defun r4-filter-toggle-tag-at-mouse (event)
  "Toggle the tag clicked in EVENT in the filter of its buffer."
  (interactive "e")
  (let* ((position (event-start event))
         (object (posn-string position))
         (tag (if object
                  (get-text-property (cdr object) 'r4-tag (car object))
                (get-text-property (posn-point position) 'r4-tag
                                   (window-buffer (posn-window position))))))
    (with-current-buffer (window-buffer (posn-window position))
      (when (and tag r4-filter-toggle-function)
        (funcall r4-filter-toggle-function (list tag))))))

(defvar-keymap r4-filter-tag-map
  :doc "Mouse keys of the tags, toggling them in the filter."
  "<mouse-1>" #'r4-filter-toggle-tag-at-mouse
  "<mode-line> <mouse-1>" #'r4-filter-toggle-tag-at-mouse)

(defun r4-filter-tag-button (tag help)
  "Return TAG as #TAG, toggling it in the filter when clicked, showing HELP."
  (propertize (concat "#" tag)
              'r4-tag tag
              'face 'r4-tag
              'mouse-face 'highlight
              'help-echo help
              'keymap r4-filter-tag-map
              'local-map r4-filter-tag-map))

(defun r4-filter-tags-string (tags)
  "Return the TAGS of a track, to click to filter by them."
  (mapconcat (lambda (tag) (r4-filter-tag-button (downcase tag) "mouse-1: filter by this tag"))
             (if (stringp tags) (split-string tags " " t) tags)
             " "))

(defun r4-filter-mode-line (text shown total)
  "Return the mode line of the filter TEXT, showing SHOWN of TOTAL tracks.
Its tags remove themselves when clicked."
  (when text
    (concat " "
            (mapconcat (lambda (token)
                         (if (string-match "\\`#\\(.+\\)" token)
                             (r4-filter-tag-button (downcase (match-string 1 token))
                                                   "mouse-1: remove this tag")
                           (string-replace "%" "%%" token)))
                       (split-string text "[ \t\n]+" t)
                       " ")
            (format " %d/%d" shown total))))

(provide 'r4-filter)

;;; r4-filter.el ends here
