;;; r4-bookmark.el --- Bookmark Radio4000 lists  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Bookmark any list of tracks or channels with `bookmark-set' (C-x r
;; m), and open it again with `bookmark-jump' (C-x r b): a search, a
;; channel, a filter, the feed.  They are the saved views of
;; radio4000.com, in Emacs.

;;; Code:

(require 'bookmark)
(require 'r4-tracks)
(require 'r4-channels)
(require 'r4-search)

(defun r4-bookmark--name ()
  "Return the default bookmark name of the current list."
  (string-trim (buffer-name) "\\*" "\\*"))

(defun r4-bookmark-make-tracks-record ()
  "Return a bookmark record of the current list of tracks."
  `(,(r4-bookmark--name)
    (handler . r4-bookmark-jump-tracks)
    (r4-source . ,r4-tracks--source)
    (r4-filter . ,r4-tracks--filter-text)
    (position . ,(point))))

(defun r4-bookmark-jump-tracks (bookmark)
  "Show the list of tracks of BOOKMARK."
  (let ((filter (bookmark-prop-get bookmark 'r4-filter)))
    (set-buffer (r4-tracks-buffer (bookmark-prop-get bookmark 'r4-source)))
    (when filter
      (r4-tracks-set-filter filter))))

(defun r4-bookmark-make-channels-record ()
  "Return a bookmark record of the current list of channels."
  (unless r4-channels--command
    (user-error "This list of channels cannot be bookmarked"))
  `(,(r4-bookmark--name)
    (handler . r4-bookmark-jump-channels)
    (r4-command . ,r4-channels--command)
    (position . ,(point))))

(defun r4-bookmark-jump-channels (bookmark)
  "Show the list of channels of BOOKMARK."
  (let ((command (bookmark-prop-get bookmark 'r4-command)))
    (unless (and (symbolp (car command)) (string-prefix-p "r4-" (symbol-name (car command))))
      (user-error "Not an r4 bookmark: %S" command))
    (set-buffer (save-window-excursion
                  (apply (car command) (cdr command))
                  (current-buffer)))))

(add-hook 'r4-tracks-mode-hook
          (lambda () (setq-local bookmark-make-record-function #'r4-bookmark-make-tracks-record)))
(add-hook 'r4-channels-mode-hook
          (lambda () (setq-local bookmark-make-record-function #'r4-bookmark-make-channels-record)))

(provide 'r4-bookmark)

;;; r4-bookmark.el ends here
