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
(require 'r4-files)
(require 'r4-download)
(require 'r4-downloads)
(require 'r4-columns)

(defvar r4-channels--cache nil
  "List of all `r4-channel', filled by the last full channel listing.")

(defvar-local r4-channels--query nil
  "Full-text search of the channels buffer, or nil for all channels.")

(defvar-local r4-channels--fetch nil
  "Function fetching the channels of the buffer, or nil for a search.
It is called with a callback, to call with the list of channels.")

(defvar-local r4-channels--channels nil
  "List of the `r4-channel' of the buffer, as fetched.")

(defvar-local r4-channels--filter nil
  "Predicate of the channels shown, or nil to show all.")

(defun r4-channels-redisplay ()
  "Redisplay the channels of the buffer, without fetching them."
  (let ((channels (if r4-channels--filter
                      (seq-filter r4-channels--filter r4-channels--channels)
                    r4-channels--channels)))
    (setq tabulated-list-entries (r4-channels--entries channels))
    (tabulated-list-print t)
    (setq mode-line-process
          (when r4-channels--filter
            (format " %d/%d" (length channels) (length r4-channels--channels))))))

(defvar-local r4-channels--command nil
  "Command and arguments showing the channels of the buffer, for bookmarks.")

(defvar-local r4-channels--name nil
  "Name of the channels of the buffer, such as \"following @oskar\".")

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
  (let* ((buffer (current-buffer))
         (query r4-channels--query)
         (fetch r4-channels--fetch)
         (name (or r4-channels--name
                   (if query (format "channels matching \"%s\"" query) "channels")))
         (callback
          (lambda (channels)
            (unless (or query fetch) (r4-channels--set-cache channels))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (setq r4-channels--channels channels)
                (r4-channels-redisplay)
                (message "%d %s" (length channels) name))))))
    (setq mode-line-process " loading…")
    (force-mode-line-update)
    (let ((r4-api-error-function (lambda (error) (r4-list-fetch-failed buffer error))))
      (condition-case err
          (if fetch
              (funcall fetch callback)
            (r4-api-channels callback query))
        ((error user-error) (r4-list-fetch-failed buffer (error-message-string err)))))))

;;;; Completion

(defcustom r4-channels-cache-file (locate-user-emacs-file "r4-channels.eld")
  "File caching the list of channels, for instant completion, or nil."
  :type '(choice (file :tag "File") (const :tag "No cache" nil))
  :group 'r4)

(defun r4-channels--set-cache (channels)
  "Remember CHANNELS, the list of all channels, in memory and on disk."
  (setq r4-channels--cache channels)
  (when r4-channels-cache-file
    (let ((print-length nil) (print-level nil))
      (with-temp-file r4-channels-cache-file
        (prin1 channels (current-buffer)))))
  channels)

(defun r4-channels--load-cache ()
  "Load the channels of `r4-channels-cache-file', and refresh them soon."
  (when (and r4-channels-cache-file (file-readable-p r4-channels-cache-file))
    (when-let* ((channels (ignore-errors
                            (with-temp-buffer
                              (insert-file-contents r4-channels-cache-file)
                              (read (current-buffer))))))
      (setq r4-channels--cache channels)
      (let ((r4-api-error-function #'ignore))
        (r4-api-channels #'r4-channels--set-cache))
      channels)))

(defun r4-channels--all ()
  "Return all channels, from the cache, or else fetching them."
  (or r4-channels--cache
      (r4-channels--load-cache)
      (progn (message "Fetching channels…")
             (r4-channels--set-cache (r4-api-channels-sync)))))

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

(defun r4-channels-shuffle ()
  "Shuffle play the channel at point."
  (interactive)
  (setq r4-player-shuffle t)
  (r4-play-channel (r4-channel-slug (r4-channels--at-point))))

(defun r4-channels-copy-url ()
  "Copy the radio4000.com URL of the channel at point."
  (interactive)
  (let ((url (r4-channel-web-url (r4-channels--at-point))))
    (kill-new url)
    (message "Copied %s" url)))

(defun r4-channels-download ()
  "Download all the tracks of the channel at point."
  (interactive)
  (r4-download-channel (r4-channel-slug (r4-channels--at-point))))

(defun r4-channels-browse ()
  "Open the channel at point on radio4000.com."
  (interactive)
  (browse-url (r4-channel-web-url (r4-channels--at-point))))

(declare-function r4-describe-channel "r4-describe")

(defun r4-channels-describe ()
  "Describe the channel at point."
  (interactive)
  (r4-describe-channel (r4-channels--at-point)))

(defvar-keymap r4-channels-mode-map
  :doc "Keymap for `r4-channels-mode'."
  :parent (r4-player-list-map)
  "RET" #'r4-channels-open
  "P" #'r4-channels-play
  "z" #'r4-channels-shuffle
  "D" #'r4-channels-download
  "L" #'r4-downloads
  "i" #'r4-channels-describe
  "o" #'r4-channels-browse
  "w" #'r4-channels-copy-url
  "s" #'r4-search-channels)

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

(defun r4-channels-show (name fetch &optional command)
  "Show the channels NAME, fetched by FETCH, see `r4-channels--fetch'.
COMMAND, a list of a command and its arguments, shows them again."
  (with-current-buffer (get-buffer-create (format "*r4 %s*" name))
    (unless (derived-mode-p 'r4-channels-mode) (r4-channels-mode))
    (setq r4-channels--name name
          r4-channels--fetch fetch
          r4-channels--command command)
    (revert-buffer)
    (pop-to-buffer-same-window (current-buffer))))

(defun r4-channels--show (query)
  "Show the channels matching QUERY, or all channels if nil."
  (with-current-buffer (get-buffer-create
                        (if query (format "*r4 channels \"%s\"*" query) "*r4 channels*"))
    (unless (derived-mode-p 'r4-channels-mode) (r4-channels-mode))
    (setq r4-channels--query query
          r4-channels--command (if query (list 'r4-search-channels query) (list 'r4-channels)))
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
  "Download all the tracks of the channel SLUG.
With a prefix argument, also retry the tracks that failed before."
  (interactive (list (r4-read-channel "Download channel: @")))
  (message "Fetching @%s…" slug)
  (r4-api-channel
   slug
   (lambda (channel)
     (unless channel (user-error "No channel @%s" slug))
     (r4-api-tracks
      (lambda (tracks)
        (when (yes-or-no-p (format "Download %d %s of @%s to %s? "
                                   (length tracks)
                                   (if r4-download-video "videos" "tracks")
                                   slug (r4-files-channel-directory slug)))
          (r4-download-tracks tracks :channel channel :channel-tracks tracks
                              :retry current-prefix-arg)))
      :slug slug))))

(provide 'r4-channels)

;;; r4-channels.el ends here
