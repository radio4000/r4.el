;;; r4-channel-page.el --- The page of a Radio4000 channel  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The page of a channel, like /@slug on radio4000.com: its image,
;; name, link and description, where #tags, @mentions and links are
;; buttons, buttons to play it, its pages (tracks, following,
;; followers, mentions), its most used tags, and its latest tracks.
;; RET in channel lists opens it.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'r4-models)
(require 'r4-api)
(require 'r4-files)
(require 'r4-player)
(require 'r4-tracks)
(require 'r4-channels)
(require 'r4-filter)
(require 'r4-follows)
(require 'r4-auto-radio)
(require 'r4-synced)
(require 'r4-search)
(require 'r4-local)

(defcustom r4-channel-image-size 250
  "Size in pixels of the channel image on channel pages.
0 hides the image."
  :type 'natnum
  :group 'r4)

(defcustom r4-channel-page-tracks 5
  "Number of latest tracks shown on channel pages, like radio4000.com."
  :type 'natnum
  :group 'r4)

(defcustom r4-channel-page-tags 20
  "Number of most used tags shown on channel pages."
  :type 'natnum
  :group 'r4)

(defvar-local r4-channel-page--channel nil
  "The `r4-channel' of the page.")

(defvar-local r4-channel-page--tracks nil
  "The tracks of the channel of the page, once fetched.")

(defvar-local r4-channel-page--image nil
  "The image of the channel of the page, once fetched, or `none'.")

;;;; Commands

(defun r4-channel-page--slug ()
  "Return the slug of the channel of the page."
  (r4-channel-slug (or r4-channel-page--channel (user-error "Not a channel page"))))

(defun r4-channel-page-open-tracks (slug &optional filter)
  "Show the tracks of the channel SLUG, with the search FILTER if non-nil."
  (let ((buffer (r4-tracks-buffer (list :slug slug))))
    (when filter
      (with-current-buffer buffer (r4-tracks-set-filter filter)))
    (pop-to-buffer-same-window buffer)))

(defun r4-channel-page-tracks (&optional filter)
  "Show the tracks of the channel, with the search FILTER if non-nil."
  (interactive)
  (r4-channel-page-open-tracks (r4-channel-page--slug) filter))

(defun r4-channel-page-tag (tag)
  "Show the tracks of the channel with TAG."
  (interactive
   (list (completing-read "Tracks with tag: #"
                          (mapcar #'car (r4-filter-tag-counts r4-channel-page--tracks))
                          nil t)))
  (r4-channel-page-tracks (concat "#" tag)))

(defun r4-channel-page-play ()
  "Play the channel."
  (interactive)
  (if r4-channel-page--tracks
      (r4-player-play r4-channel-page--tracks)
    (r4-play-channel (r4-channel-page--slug))))

(defun r4-channel-page-shuffle ()
  "Turn shuffle on and play the channel."
  (interactive)
  (setq r4-player-shuffle t)
  (r4-channel-page-play))

(defun r4-channel-page-auto-radio ()
  "Tune in to the auto radio of the channel."
  (interactive)
  (r4-auto-radio (r4-channel-page--slug)))

(defun r4-channel-page-follow ()
  "Follow the channel with your channel, or stop following it."
  (interactive)
  (r4-follows-toggle r4-channel-page--channel))

(defun r4-channel-page-following ()
  "List the channels the channel follows."
  (interactive)
  (r4-following (r4-channel-page--slug)))

(defun r4-channel-page-followers ()
  "List the channels following the channel."
  (interactive)
  (r4-followers (r4-channel-page--slug)))

(defun r4-channel-page-mentions ()
  "List the tracks mentioning the channel."
  (interactive)
  (r4-mentions (r4-channel-page--slug)))

(defun r4-channel-page-download ()
  "Download the channel."
  (interactive)
  (r4-download-channel (r4-channel-page--slug)))

(defun r4-channel-page-save-locally ()
  "Save the channel locally, as a backup."
  (interactive)
  (r4-local-save-channel (r4-channel-page--slug)))

(defun r4-channel-page-toggle-sync ()
  "Start or stop syncing the channel locally."
  (interactive)
  (r4-toggle-synced-channel (r4-channel-page--slug))
  (r4-channel-page--render))

(defun r4-channel-page-browse ()
  "Open the channel on radio4000.com."
  (interactive)
  (browse-url (r4-channel-web-url r4-channel-page--channel)))

(defun r4-channel-page-copy-url ()
  "Copy the radio4000.com URL of the channel."
  (interactive)
  (kill-new (r4-channel-web-url r4-channel-page--channel))
  (message "Copied %s" (r4-channel-web-url r4-channel-page--channel)))

;;;; Content

(defun r4-channel-page-insert-linked (text slug)
  "Insert TEXT, with its links, #tags and @mentions as buttons.
Its #tags show the tracks of the channel SLUG with that tag."
  (let ((start 0))
    (while (string-match "https?://[^ \t\n]+\\|#[[:alnum:]_-]+\\|@[[:alnum:]_-]+"
                         text start)
      (let ((match (match-string 0 text))
            (end (match-end 0)))
        (insert (substring text start (match-beginning 0)))
        (pcase (aref match 0)
          (?# (let ((filter (downcase match)))
                (insert-text-button match 'action (lambda (_) (r4-channel-page-open-tracks
                                                               slug filter))
                                    'follow-link t
                                    'help-echo (format "Tracks with %s" match))))
          (?@ (let ((slug (substring match 1)))
                (insert-text-button match 'action (lambda (_) (r4-channel-page slug))
                                    'follow-link t
                                    'help-echo (format "Visit %s" match))))
          (_ (insert-text-button match 'type 'help-url 'help-args (list match))))
        (setq start end)))
    (insert (substring text start))))

(defun r4-channel-page--field (name)
  "Insert the field NAME, right aligned, like `describe-package'."
  (insert (make-string (max 0 (- 12 (string-width name))) ?\s)
          (propertize (concat name ": ") 'face 'r4-heading)))

(defun r4-channel-page--insert-wrapped (items)
  "Insert ITEMS, functions inserting a text and returning its width.
Wrap them under the field values."
  (let ((column 14))
    (dolist (item items)
      (let ((start (point))
            (width (funcall item)))
        (when (and (> (+ column width) fill-column) (> column 14))
          (save-excursion (goto-char start) (insert "\n" (make-string 14 ?\s)))
          (setq column 14))
        (insert "  ")
        (cl-incf column (+ width 2))))
    (insert "\n")))

(defun r4-channel-page--insert-tags ()
  "Insert the most used tags of the channel, with their counts."
  (when-let* ((counts (seq-take (r4-filter-tag-counts r4-channel-page--tracks)
                                r4-channel-page-tags)))
    (r4-channel-page--field "Tags")
    (r4-channel-page--insert-wrapped
     (mapcar (pcase-lambda (`(,tag . ,count))
               (lambda ()
                 (insert-text-button (concat "#" tag)
                                     'action (lambda (_) (r4-channel-page-tag tag))
                                     'follow-link t
                                     'face '(r4-tag button)
                                     'help-echo (format "Tracks with #%s" tag))
                 (insert " " (propertize (number-to-string count) 'face 'r4-number))
                 (+ (length tag) 2 (length (number-to-string count)))))
             counts))))

(defun r4-channel-page--insert-tracks ()
  "Insert the latest tracks of the channel."
  (insert (propertize "Latest tracks" 'face 'r4-heading) "\n")
  (cond ((null r4-channel-page--tracks)
         (insert "  " (propertize (if (eql (r4-channel-track-count r4-channel-page--channel) 0)
                                      "No tracks"
                                    "Loading tracks…")
                                  'face 'shadow)
                 "\n"))
        (t
         (cl-loop for track in r4-channel-page--tracks
                  for index from 0 below r4-channel-page-tracks
                  do (let ((index index))
                       (insert "  ")
                       (insert-text-button
                        (truncate-string-to-width (r4-one-line (r4-track-title track))
                                                  (max 20 (- fill-column 30)) nil ?\s "…")
                        'action (lambda (_) (r4-player-play r4-channel-page--tracks index))
                        'follow-link t
                        'face '(r4-title button)
                        'help-echo "Play from this track")
                       (insert "  " (propertize (r4-format-duration (r4-track-duration track))
                                                'face 'r4-number)
                               "  " (propertize (r4-format-date (r4-track-created-at track))
                                                'face 'r4-date)
                               "\n")))
         (insert "  ")
         (r4-faces-insert-key "t" (format "See all %d tracks" (length r4-channel-page--tracks))
                              #'r4-channel-page-tracks)
         (insert "\n"))))

(defun r4-channel-page--insert-image (channel)
  "Insert the image of CHANNEL, fetching it first."
  (cond ((eq (car-safe r4-channel-page--image) 'image)
         (insert-image r4-channel-page--image "[image]")
         (insert "\n\n"))
        ((null r4-channel-page--image)
         (setq r4-channel-page--image 'none)
         (when-let* (((display-images-p))
                     ((> r4-channel-image-size 0))
                     (url (r4-channel-image-url channel r4-channel-image-size)))
           (let ((buffer (current-buffer)))
             (r4-api-fetch-image url
                                 (lambda (image)
                                   (when (and image (buffer-live-p buffer))
                                     (with-current-buffer buffer
                                       (setq r4-channel-page--image image)
                                       (r4-channel-page--render))))))))))

(defun r4-channel-page--insert (channel)
  "Insert the page of CHANNEL, like `describe-package'."
  (let ((slug (r4-channel-slug channel)))
    (r4-channel-page--insert-image channel)
    (insert (propertize (or (r4-channel-name channel) slug) 'face 'r4-big-heading) "\n"
            (propertize (concat "@" slug) 'face 'r4-slug) "\n\n")
    (when-let* ((url (r4-channel-url channel))
                ((not (string-blank-p url))))
      (r4-channel-page--field "Website")
      (insert-text-button url 'type 'help-url 'help-args (list url))
      (insert "\n"))
    (r4-channel-page--field "Tracks")
    (insert (propertize (format "%s" (or (r4-channel-track-count channel) 0)) 'face 'r4-number))
    (when-let* ((latest (r4-channel-latest-track-at channel)))
      (insert ", the latest on " (propertize (r4-format-date latest) 'face 'r4-date)))
    (insert "\n")
    (r4-channel-page--field "Created")
    (insert (propertize (r4-format-date (r4-channel-created-at channel)) 'face 'r4-date) "\n")
    (when (and (r4-channel-latitude channel) (r4-channel-longitude channel))
      (r4-channel-page--field "Location")
      (insert-text-button (format "%.2f, %.2f" (r4-channel-latitude channel)
                                  (r4-channel-longitude channel))
                          'type 'help-url 'help-args (list (r4-channel-map-url channel)))
      (insert "\n"))
    (r4-channel-page--field "Links")
    (insert-text-button "radio4000.com" 'type 'help-url
                        'help-args (list (r4-channel-web-url channel)))
    (insert "  ")
    (insert-text-button "RSS" 'type 'help-url 'help-args (list (r4-channel-rss-url channel)))
    (insert "\n")
    (r4-channel-page--field "Locally")
    (insert (cond ((r4-synced-p slug) (propertize "synced" 'face 'r4-synced))
                  ((r4-local-p slug) "saved")
                  (t (propertize "not synced" 'face 'shadow)))
            "\n")
    (when-let* ((description (r4-channel-description channel))
                ((not (string-blank-p description))))
      (r4-channel-page--field "Description")
      (let ((start (point)))
        (r4-channel-page-insert-linked description slug)
        (add-face-text-property start (point) 'r4-description t)
        (let ((indent-tabs-mode nil))
          (indent-rigidly (save-excursion (goto-char start) (line-beginning-position 2))
                          (point) 14)))
      (insert "\n"))
    (r4-channel-page--insert-tags)
    (insert "\n")
    (dolist (row `((("P" "Play" r4-channel-page-play)
                    ("z" "Shuffle" r4-channel-page-shuffle)
                    ("R" "Auto radio" r4-channel-page-auto-radio))
                   (("t" ,(format "Tracks %s" (or (r4-channel-track-count channel) ""))
                     r4-channel-page-tracks)
                    ("#" "Tags" r4-channel-page-tag)
                    ("F" "Following" r4-channel-page-following)
                    ("r" "Followers" r4-channel-page-followers)
                    ("M" "Mentions" r4-channel-page-mentions))
                   (("f" "Follow" r4-channel-page-follow)
                    ("y" ,(if (r4-synced-p slug) "Stop syncing" "Sync locally")
                     r4-channel-page-toggle-sync)
                    ("D" "Download" r4-channel-page-download)
                    ("C" "Save locally" r4-channel-page-save-locally)
                    ("o" "Open on radio4000.com" r4-channel-page-browse))))
      (insert "  ")
      (pcase-dolist (`(,key ,label ,command) row)
        (r4-faces-insert-key key label command)
        (insert "   "))
      (insert "\n"))
    (insert "\n")
    (r4-channel-page--insert-tracks)))

(defun r4-channel-page--render ()
  "Redraw the channel page, keeping the position."
  (let ((inhibit-read-only t)
        (position (point)))
    (erase-buffer)
    (r4-channel-page--insert r4-channel-page--channel)
    (goto-char (min position (point-max)))))

(defun r4-channel-page--fetch-tracks ()
  "Fetch the tracks of the channel of the page, then show them."
  (let ((buffer (current-buffer)))
    (setq r4-channel-page--tracks nil)
    (r4-api-tracks (lambda (tracks)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (setq r4-channel-page--tracks tracks)
                         (r4-channel-page--render))))
                   :slug (r4-channel-page--slug))))

(defun r4-channel-page-revert (&rest _)
  "Fetch the channel and its tracks again."
  (let ((buffer (current-buffer)))
    (r4-api-channel (r4-channel-page--slug)
                    (lambda (channel)
                      (when (and channel (buffer-live-p buffer))
                        (with-current-buffer buffer
                          (setq r4-channel-page--channel channel
                                r4-channel-page--image nil)
                          (r4-channel-page--render)
                          (r4-channel-page--fetch-tracks)))))))

(defvar-keymap r4-channel-page-mode-map
  :doc "Keymap for `r4-channel-page-mode'."
  :parent (make-composed-keymap r4-player-map special-mode-map)
  "TAB" #'forward-button
  "<backtab>" #'backward-button
  "t" #'r4-channel-page-tracks
  "#" #'r4-channel-page-tag
  "P" #'r4-channel-page-play
  "z" #'r4-channel-page-shuffle
  "R" #'r4-channel-page-auto-radio
  "f" #'r4-channel-page-follow
  "F" #'r4-channel-page-following
  "r" #'r4-channel-page-followers
  "M" #'r4-channel-page-mentions
  "y" #'r4-channel-page-toggle-sync
  "D" #'r4-channel-page-download
  "C" #'r4-channel-page-save-locally
  "o" #'r4-channel-page-browse
  "w" #'r4-channel-page-copy-url)

(define-derived-mode r4-channel-page-mode special-mode "r4 channel"
  "Major mode of the page of a Radio4000 channel.

\\{r4-channel-page-mode-map}"
  (setq-local revert-buffer-function #'r4-channel-page-revert))

;;;###autoload
(defun r4-channel-page (channel)
  "Show the page of CHANNEL, an `r4-channel' or a slug.
Like /@slug on radio4000.com: its description, buttons to play it and
visit its pages, its tags and its latest tracks."
  (interactive (list (r4-read-channel "Visit channel: @")))
  (if (stringp channel)
      (r4-api-channel channel
                      (lambda (found)
                        (if found
                            (r4-channel-page found)
                          (message "No channel @%s" channel))))
    (with-current-buffer (get-buffer-create
                          (format "*r4 @%s channel*" (r4-channel-slug channel)))
      (r4-channel-page-mode)
      (setq r4-channel-page--channel channel)
      (r4-channel-page--render)
      (goto-char (point-min))
      (r4-channel-page--fetch-tracks)
      (pop-to-buffer-same-window (current-buffer)))))

;;;###autoload
(defalias 'r4-describe-channel #'r4-channel-page)

(defun r4-channels-visit ()
  "Show the page of the channel at point."
  (interactive)
  (r4-channel-page (r4-channels--at-point)))

(setq r4-tracks-visit-channel-function #'r4-channel-page)

;;;; Going up, see `r4-up'

(defvar r4-up-function)

(defun r4-channel-page--up-from-tracks ()
  "Go up from a list of tracks: to its channel page, else home."
  (let ((slug (plist-get r4-tracks--source :slug)))
    (if (and slug (not (plist-get r4-tracks--source :local))
             (not (plist-get r4-tracks--source :query)))
        (r4-channel-page slug)
      (r4))))

(defun r4-channel-page--setup-up ()
  "Make `r4-up' go up from the current buffer."
  (setq-local r4-up-function
              (if (derived-mode-p 'r4-tracks-mode)
                  #'r4-channel-page--up-from-tracks
                #'r4-channels)))

(add-hook 'r4-tracks-mode-hook #'r4-channel-page--setup-up)
(add-hook 'r4-channel-page-mode-hook #'r4-channel-page--setup-up)

(declare-function r4 "r4-home")

(keymap-set r4-channels-mode-map "RET" #'r4-channels-visit)
(keymap-set r4-channels-mode-map "T" #'r4-channels-open)

(provide 'r4-channel-page)

;;; r4-channel-page.el ends here
