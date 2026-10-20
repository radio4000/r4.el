;;; r4-channels.el --- List Radio4000 channels  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A tabulated list of Radio4000 channels, channel completion with
;; annotations, and a channel description buffer.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'r4-models)
(require 'r4-api)
(require 'r4-tracks)
(require 'r4-download)
(require 'r4-columns)

(defvar r4-channels--cache nil
  "List of all `r4-channel', filled by the last full channel listing.")

(defvar-local r4-channels--query nil
  "Full-text search of the channels buffer, or nil for all channels.")

(defconst r4-channels-column-specs
  `((slug . ,(r4-column-create :name "Slug" :width 24 :key #'r4-channel-slug))
    (name . ,(r4-column-create :name "Name" :width 30 :key #'r4-channel-name))
    (tracks . ,(r4-column-create :name "Tracks" :width 7 :key #'r4-channel-track-count
                                 :right-align t))
    (latest . ,(r4-column-create :name "Active" :width 10 :key #'r4-channel-latest-track-at
                                 :format #'r4-format-date))
    (created . ,(r4-column-create :name "Created" :width 10 :key #'r4-channel-created-at
                                  :format #'r4-format-date))
    (updated . ,(r4-column-create :name "Updated" :width 10 :key #'r4-channel-updated-at
                                  :format #'r4-format-date))
    (description . ,(r4-column-create :name "Description" :width 40
                                      :key #'r4-channel-description))
    (url . ,(r4-column-create :name "Link" :width 30 :key #'r4-channel-url)))
  "Columns available in channel lists, see `r4-channels-columns'.
The Active column is the date of the latest track.")

(defcustom r4-channels-columns '(slug name tracks latest created updated description)
  "Columns of the channel lists, among `r4-channels-column-specs'."
  :type (r4-columns-custom-type r4-channels-column-specs)
  :group 'r4)

(defvar-local r4-channels--columns nil
  "Columns of the buffer, from `r4-channels-columns'.")

(defun r4-channels--entries (channels)
  "Return the `tabulated-list-entries' of CHANNELS."
  (mapcar (lambda (channel)
            (list channel (r4-columns-row channel r4-channels--columns
                                          r4-channels-column-specs)))
          channels))

(defun r4-channels-revert (&rest _)
  "Fetch the channels of the buffer again."
  (let ((buffer (current-buffer))
        (query r4-channels--query))
    (message "Fetching channels…")
    (r4-api-channels
     (lambda (channels)
       (unless query (setq r4-channels--cache channels))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq tabulated-list-entries (r4-channels--entries channels))
           (tabulated-list-print t)
           (message "%d channels%s" (length channels)
                    (if query (format " matching \"%s\"" query) "")))))
     query)))

;;;; Completion

(defun r4-channels--all ()
  "Return all channels, fetching them if needed."
  (or r4-channels--cache
      (progn (message "Fetching channels…")
             (setq r4-channels--cache (r4-api-channels-sync)))))

(defvar r4-channel-history nil
  "Minibuffer history of channel slugs.")

(defun r4-read-channel (prompt)
  "Read a channel slug with completion, using PROMPT."
  (let* ((channels (r4-channels--all))
         (table (make-hash-table :test #'equal)))
    (dolist (channel channels)
      (puthash (r4-channel-slug channel) channel table))
    (completing-read
     prompt
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (category . r4-channel)
             (display-sort-function . identity)
             (annotation-function
              . ,(lambda (slug)
                   (when-let* ((channel (gethash slug table)))
                     (format "  %s (%s tracks)  %s"
                             (propertize (r4-one-line (r4-channel-name channel)) 'face 'bold)
                             (or (r4-channel-track-count channel) 0)
                             (propertize (truncate-string-to-width
                                          (r4-one-line (r4-channel-description channel)) 60)
                                         'face 'completions-annotations))))))
         (complete-with-action action (mapcar #'r4-channel-slug channels) string predicate)))
     nil nil nil 'r4-channel-history)))

;;;; Commands

(defun r4-channels--at-point ()
  "Return the channel at point, or signal an error."
  (or (tabulated-list-get-id) (user-error "No channel at point")))

(defun r4-channels-open ()
  "Open the tracks of the channel at point."
  (interactive)
  (r4-open-channel (r4-channel-slug (r4-channels--at-point))))

(defun r4-channels-play ()
  "Play the channel at point."
  (interactive)
  (r4-play-channel (r4-channel-slug (r4-channels--at-point))))

(defun r4-channels-download ()
  "Download all the tracks of the channel at point."
  (interactive)
  (r4-download-channel (r4-channel-slug (r4-channels--at-point))))

(defun r4-channels-browse ()
  "Open the channel at point on radio4000.com."
  (interactive)
  (browse-url (r4-channel-web-url (r4-channels--at-point))))

(defun r4-channels-describe ()
  "Describe the channel at point."
  (interactive)
  (r4-describe-channel (r4-channels--at-point)))

(defvar-keymap r4-channels-mode-map
  :doc "Keymap for `r4-channels-mode'."
  "RET" #'r4-channels-open
  "P" #'r4-channels-play
  "D" #'r4-channels-download
  "o" #'r4-channels-browse
  "i" #'r4-channels-describe
  "s" #'r4-search-channels
  "t" #'r4-search-tracks
  "SPC" #'r4-player-toggle-pause
  ">" #'r4-player-next
  "<" #'r4-player-previous
  "k" #'r4-player-stop
  "Q" #'r4-queue
  "L" #'r4-downloads)

(define-derived-mode r4-channels-mode tabulated-list-mode "r4 channels"
  "Major mode listing Radio4000 channels.

Sort by the column at point with \\[tabulated-list-sort], or by
clicking a column header.  Choose the columns with
`r4-channels-columns'.

\\{r4-channels-mode-map}"
  (setq r4-channels--columns (r4-columns-valid r4-channels-columns r4-channels-column-specs)
        tabulated-list-format (r4-columns-list-format r4-channels--columns
                                                      r4-channels-column-specs)
        tabulated-list-padding 2)
  (setq-local revert-buffer-function #'r4-channels-revert)
  (tabulated-list-init-header))

(defun r4-channels--show (query)
  "Show the channels matching QUERY, or all channels if nil."
  (with-current-buffer (get-buffer-create
                        (if query (format "*r4 channels \"%s\"*" query) "*r4 channels*"))
    (unless (derived-mode-p 'r4-channels-mode) (r4-channels-mode))
    (setq r4-channels--query query)
    (revert-buffer)
    (pop-to-buffer-same-window (current-buffer))))

;;;###autoload
(defun r4-channels ()
  "List all Radio4000 channels, most recently active first."
  (interactive)
  (r4-channels--show nil))

;;;###autoload
(defun r4-search-channels (query)
  "List the Radio4000 channels matching QUERY."
  (interactive (list (read-string "Search channels: ")))
  (r4-channels--show (if (string-blank-p query) nil query)))

;;;###autoload
(defun r4-open-channel (slug)
  "Show the tracks of the channel SLUG."
  (interactive (list (r4-read-channel "Channel: @")))
  (r4-tracks-show (list :slug slug)))

;;;###autoload
(defun r4-play-channel (slug)
  "Play all the tracks of the channel SLUG."
  (interactive (list (r4-read-channel "Play channel: @")))
  (message "Fetching @%s…" slug)
  (r4-api-tracks (lambda (tracks) (r4-player-play tracks)) :slug slug))

;;;###autoload
(defun r4-download-channel (slug)
  "Download all the tracks of the channel SLUG."
  (interactive (list (r4-read-channel "Download channel: @")))
  (message "Fetching @%s…" slug)
  (r4-api-tracks (lambda (tracks)
                   (when (yes-or-no-p (format "Download %d tracks of @%s to %s? "
                                              (length tracks) slug
                                              (r4-download-channel-directory slug)))
                     (r4-download-tracks tracks tracks)))
                 :slug slug))

;;;###autoload
(defun r4-describe-channel (channel)
  "Describe CHANNEL, an `r4-channel' or a slug."
  (interactive (list (r4-read-channel "Describe channel: @")))
  (if (stringp channel)
      (r4-api-channel channel
                      (lambda (found)
                        (if found
                            (r4-describe-channel found)
                          (message "No channel @%s" channel))))
    (with-help-window (format "*r4 @%s info*" (r4-channel-slug channel))
      (with-current-buffer standard-output
        (insert (propertize (or (r4-channel-name channel) "") 'face 'bold)
                "  @" (r4-channel-slug channel) "\n\n")
        (when-let* ((description (r4-channel-description channel)))
          (insert description "\n\n"))
        (insert (format "Tracks:      %s\n" (or (r4-channel-track-count channel) 0))
                (format "Created:     %s\n" (r4-format-date (r4-channel-created-at channel)))
                (format "Updated:     %s\n" (r4-format-date (r4-channel-updated-at channel)))
                (format "Last track:  %s\n" (r4-format-date (r4-channel-latest-track-at channel))))
        (unless (string-empty-p (or (r4-channel-url channel) ""))
          (insert "Link:        ")
          (insert-button (r4-channel-url channel) 'type 'help-url
                         'help-args (list (r4-channel-url channel)))
          (insert "\n"))
        (insert "Web:         ")
        (insert-button (r4-channel-web-url channel) 'type 'help-url
                       'help-args (list (r4-channel-web-url channel)))
        (insert "\nDownloads:   " (r4-download-channel-directory (r4-channel-slug channel)) "\n\n")
        (insert-text-button "[Open tracks]" 'action
                            (lambda (_) (r4-open-channel (r4-channel-slug channel))))
        (insert "  ")
        (insert-text-button "[Play]" 'action
                            (lambda (_) (r4-play-channel (r4-channel-slug channel))))
        (insert "  ")
        (insert-text-button "[Download]" 'action
                            (lambda (_) (r4-download-channel (r4-channel-slug channel))))))))

(provide 'r4-channels)

;;; r4-channels.el ends here
