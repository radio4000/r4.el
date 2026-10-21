;;; r4-queue.el --- The Radio4000 play queue  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The list of the tracks in the play queue, see r4-player.el.  Like
;; the queue of radio4000.com, / and # filter it by text and tags, and
;; the filter decides what plays next.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'r4-models)
(require 'r4-player)
(require 'r4-filter)
(require 'r4-columns)

(defconst r4-queue-buffer "*r4 queue*"
  "Name of the buffer listing the play queue.")

(defvar r4-queue-filter-text nil
  "Text of the filter of the queue, see `r4-queue-filter'.")

(defun r4-queue--entries ()
  "Return the `tabulated-list-entries' of the play queue, passing its filter."
  (cl-loop for track across r4-player-queue
           for index from 0
           when (or (null r4-player-queue-filter) (funcall r4-player-queue-filter track))
           collect (list index
                         (vector (if (eql index r4-player-index)
                                     (r4-faces-status "▶" 'r4-playing)
                                   "")
                                 (propertize (concat "@" (r4-track-slug track)) 'face 'r4-slug)
                                 (r4-one-line (r4-track-title track))
                                 (propertize (r4-format-duration (r4-track-duration track))
                                             'face 'r4-number)
                                 (r4-filter-tags-string (r4-track-tags track))))))

(defun r4-queue--revert ()
  "Set the entries and mode line of the queue buffer."
  (unless r4-player-queue-filter
    (setq r4-queue-filter-text nil))
  (setq tabulated-list-entries (r4-queue--entries))
  (r4-columns-show-empty (if r4-queue-filter-text
                             "No queue tracks match the filter, / changes it"
                           "The queue is empty: RET in a list of tracks plays them"))
  (setq mode-line-process (r4-filter-mode-line r4-queue-filter-text
                                               (length tabulated-list-entries)
                                               (length r4-player-queue))))

(defun r4-queue-filter (text)
  "Only show and play the queue tracks matching TEXT, such as \"#jazz\".
The next tracks play among them, like the queue of radio4000.com.  An
empty TEXT shows and plays all the tracks."
  (interactive (list (read-string "Filter the queue (@channel #tag words): "
                                  r4-queue-filter-text)))
  (setq r4-queue-filter-text (and (not (string-blank-p text)) (string-trim text))
        r4-player-queue-filter (r4-filter-matcher r4-queue-filter-text))
  (r4-queue--refresh)
  (message (if r4-queue-filter-text
               (format "Playing the queue tracks matching \"%s\"" r4-queue-filter-text)
             "Playing all the queue tracks")))

(defun r4-queue-filter-tag (tags)
  "Toggle filtering the queue by TAGS, see `r4-queue-filter'."
  (interactive (list (r4-filter-read-tags
                      (if r4-player-queue-filter
                          (seq-filter r4-player-queue-filter r4-player-queue)
                        (append r4-player-queue nil))
                      r4-queue-filter-text)))
  (r4-queue-filter (or (r4-filter-toggle-tags r4-queue-filter-text tags) "")))

(defun r4-queue--index ()
  "Return the queue index of the track at point, or signal an error."
  (or (tabulated-list-get-id) (user-error "No track at point")))

(defun r4-queue--track ()
  "Return the track at point."
  (aref r4-player-queue (r4-queue--index)))

(defun r4-queue-play ()
  "Play the queue track at point."
  (interactive)
  (r4-player--play-index (r4-queue--index)))

(defun r4-queue-remove ()
  "Remove the track at point from the queue."
  (interactive)
  (r4-player-remove (r4-queue--index)))

(defun r4-queue-browse ()
  "Open the track at point on radio4000.com."
  (interactive)
  (browse-url (r4-track-web-url (r4-queue--track))))

(defun r4-queue-copy-url ()
  "Copy the media URL of the track at point."
  (interactive)
  (kill-new (r4-track-url (r4-queue--track)))
  (message "Copied %s" (r4-track-url (r4-queue--track))))

(defun r4-queue-copy-web-url ()
  "Copy the radio4000.com URL of the track at point."
  (interactive)
  (kill-new (r4-track-web-url (r4-queue--track)))
  (message "Copied %s" (r4-track-web-url (r4-queue--track))))

(defun r4-queue--refresh ()
  "Refresh the queue buffer, if it exists."
  (when-let* ((buffer (get-buffer r4-queue-buffer)))
    (with-current-buffer buffer (revert-buffer))))

(add-hook 'r4-player-track-changed-hook #'r4-queue--refresh)
(add-hook 'r4-player-queue-changed-hook #'r4-queue--refresh)

(defvar-keymap r4-queue-mode-map
  :doc "Keymap for `r4-queue-mode'."
  :parent (r4-player-list-map)
  "RET" #'r4-queue-play
  "x" #'r4-queue-remove
  "C" #'r4-player-clear
  "o" #'r4-queue-browse
  "w" #'r4-queue-copy-url
  "W" #'r4-queue-copy-web-url
  "/" #'r4-queue-filter
  "#" #'r4-queue-filter-tag)

(define-derived-mode r4-queue-mode tabulated-list-mode "r4 queue"
  "Major mode listing the Radio4000 play queue.

\\{r4-queue-mode-map}"
  (setq tabulated-list-format [(" " 1 nil) ("slug" 20 t) ("title" 50 t)
                               ("duration" 8 nil :right-align t) ("tags" 0 nil)]
        tabulated-list-padding 1)
  (setq-local r4-filter-toggle-function #'r4-queue-filter-tag)
  (add-hook 'tabulated-list-revert-hook #'r4-queue--revert nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun r4-queue ()
  "Show the Radio4000 play queue."
  (interactive)
  (with-current-buffer (get-buffer-create r4-queue-buffer)
    (r4-queue-mode)
    (revert-buffer)
    (goto-char (point-min))
    (while (and r4-player-index (not (eobp))
                (not (eql (tabulated-list-get-id) r4-player-index)))
      (forward-line 1))
    (when (eobp) (goto-char (point-min)))
    (pop-to-buffer (current-buffer))))

(provide 'r4-queue)

;;; r4-queue.el ends here
