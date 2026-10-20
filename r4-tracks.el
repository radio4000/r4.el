;;; r4-tracks.el --- List Radio4000 tracks  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A tabulated list of tracks: the tracks of one channel, or the
;; results of a track search.  Play, mark and download tracks from it.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'r4-models)
(require 'r4-api)
(require 'r4-player)
(require 'r4-download)
(require 'r4-columns)

(defvar-local r4-tracks--source nil
  "Plist describing the tracks of the buffer.
It has the keys :slug, a channel slug, :query, a search, and :local,
non-nil for a local channel.")

(defvar-local r4-tracks--tracks nil
  "List of the `r4-track' of the buffer, in their original order.")

(defvar r4-tracks-fetch-functions nil
  "Functions fetching tracks for a source plist that is not remote.
Each function is called with the source and a callback, and returns
non-nil if it handles the source.")

(defvar r4-tracks--playing-id nil
  "Id of the track shown as playing in the track lists.")

(defun r4-tracks--status (track)
  "Return the status of TRACK: ▶ playing, ↓ downloading, ✓ downloaded."
  (cond ((equal (r4-track-id track) r4-tracks--playing-id) "▶")
        ((r4-download-pending-p track) "↓")
        ((r4-download-file track) "✓")
        (t "")))

(defconst r4-tracks-column-specs
  `((status . ,(r4-column-create :name " " :width 1 :key #'r4-tracks--status
                                 :format #'identity))
    (created . ,(r4-column-create :name "Added" :width 10 :key #'r4-track-created-at
                                  :format #'r4-format-date))
    (updated . ,(r4-column-create :name "Updated" :width 10 :key #'r4-track-updated-at
                                  :format #'r4-format-date))
    (channel . ,(r4-column-create :name "Channel" :width 16 :key #'r4-track-slug
                                  :format (lambda (slug) (concat "@" slug))))
    (title . ,(r4-column-create :name "Title" :width 50 :key #'r4-track-title))
    (duration . ,(r4-column-create :name "Time" :width 8 :key #'r4-track-duration
                                   :format #'r4-format-duration :right-align t))
    (tags . ,(r4-column-create :name "Tags" :width 16
                               :key (lambda (track) (string-join (r4-track-tags track) " "))))
    (description . ,(r4-column-create :name "Description" :width 40
                                      :key #'r4-track-description))
    (provider . ,(r4-column-create :name "Provider" :width 10 :key #'r4-track-provider))
    (url . ,(r4-column-create :name "URL" :width 45 :key #'r4-track-url)))
  "Columns available in track lists, see `r4-tracks-columns'.")

(defcustom r4-tracks-columns '(status created channel title duration tags description)
  "Columns of the track lists, among `r4-tracks-column-specs'."
  :type (r4-columns-custom-type r4-tracks-column-specs)
  :group 'r4)

(defvar-local r4-tracks--columns nil
  "Columns of the buffer, from `r4-tracks-columns'.")

(defun r4-tracks--entries ()
  "Return the `tabulated-list-entries' of the buffer tracks."
  (r4-download-with-index-cache
    (mapcar (lambda (track)
              (list track (r4-columns-row track r4-tracks--columns r4-tracks-column-specs)))
            r4-tracks--tracks)))

(defun r4-tracks--describe-source (source)
  "Return a short description of SOURCE."
  (let ((slug (plist-get source :slug))
        (query (plist-get source :query)))
    (concat (when (plist-get source :local) "local ")
            (if slug (concat "@" slug) "all channels")
            (when query (format " matching \"%s\"" query)))))

(defun r4-tracks--set (buffer tracks)
  "Display TRACKS in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq r4-tracks--tracks tracks
            tabulated-list-entries (r4-tracks--entries))
      (tabulated-list-print t)
      (message "%d tracks for %s" (length tracks) (r4-tracks--describe-source r4-tracks--source)))))

(defun r4-tracks-revert (&rest _)
  "Fetch the tracks of the buffer again."
  (let ((buffer (current-buffer))
        (source r4-tracks--source))
    (message "Fetching tracks for %s…" (r4-tracks--describe-source source))
    (let ((callback (lambda (tracks) (r4-tracks--set buffer tracks))))
      (unless (run-hook-with-args-until-success 'r4-tracks-fetch-functions source callback)
        (apply #'r4-api-tracks callback source)))))

(defun r4-tracks-update-status (ids)
  "Update the status of the tracks with IDS in all track lists."
  (when ids
    (r4-download-with-index-cache
      (r4-tracks--update-status ids))))

(defun r4-tracks--update-status (ids)
  "Update the status of the tracks with IDS, see `r4-tracks-update-status'."
  (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let* (((derived-mode-p 'r4-tracks-mode))
                    (column (cl-position 'status r4-tracks--columns)))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let ((track (tabulated-list-get-id)))
                (when (and track (member (r4-track-id track) ids))
                  (tabulated-list-set-col column (r4-tracks--status track) t)))
              (forward-line 1)))))))

(defun r4-tracks--track-changed ()
  "Show the new playing track in the track lists."
  (let ((previous r4-tracks--playing-id)
        (current (and (r4-player-live-p) (r4-player-current))))
    (setq r4-tracks--playing-id (and current (r4-track-id current)))
    (r4-tracks-update-status (delq nil (list previous r4-tracks--playing-id)))))

(defun r4-tracks--downloads-changed (tracks)
  "Show the new download status of TRACKS in the track lists."
  (r4-tracks-update-status (mapcar #'r4-track-id tracks)))

;;;; Commands

(defun r4-tracks--at-point ()
  "Return the track at point, or signal an error."
  (or (tabulated-list-get-id) (user-error "No track at point")))

(defun r4-tracks--displayed ()
  "Return the tracks in their displayed order."
  (save-excursion
    (goto-char (point-min))
    (let (tracks)
      (while (not (eobp))
        (when-let* ((track (tabulated-list-get-id))) (push track tracks))
        (forward-line 1))
      (nreverse tracks))))

(defun r4-tracks--marked ()
  "Return the marked tracks, or the track at point if none is marked."
  (or (save-excursion
        (goto-char (point-min))
        (let (tracks)
          (while (not (eobp))
            (when (eq (char-after) ?*) (push (tabulated-list-get-id) tracks))
            (forward-line 1))
          (nreverse tracks)))
      (list (r4-tracks--at-point))))

(defun r4-tracks-play ()
  "Play the track at point, then the following tracks."
  (interactive)
  (let ((track (r4-tracks--at-point))
        (tracks (r4-tracks--displayed)))
    (r4-player-play tracks (cl-position track tracks))))

(defun r4-tracks-play-all ()
  "Play all the tracks of the buffer, in random order when shuffling."
  (interactive)
  (r4-player-play (r4-tracks--displayed)))

(defun r4-tracks-shuffle ()
  "Turn shuffle on and play the tracks of the buffer."
  (interactive)
  (setq r4-player-shuffle t)
  (r4-tracks-play-all))

(defun r4-tracks-mark ()
  "Mark the track at point."
  (interactive)
  (r4-tracks--at-point)
  (tabulated-list-put-tag "*" t))

(defun r4-tracks-unmark ()
  "Unmark the track at point."
  (interactive)
  (r4-tracks--at-point)
  (tabulated-list-put-tag " " t))

(defun r4-tracks-unmark-all ()
  "Unmark all tracks."
  (interactive)
  (tabulated-list-clear-all-tags))

(defun r4-tracks-mark-all ()
  "Mark all tracks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (tabulated-list-get-id)
          (tabulated-list-put-tag "*" t)
        (forward-line 1)))))

(defun r4-tracks-download ()
  "Download the marked tracks, or the track at point."
  (interactive)
  (r4-download-tracks (r4-tracks--marked)
                      (when (plist-get r4-tracks--source :slug)
                        (unless (plist-get r4-tracks--source :query) r4-tracks--tracks)))
  (tabulated-list-clear-all-tags))

(defun r4-tracks-download-all ()
  "Download all the tracks of the buffer."
  (interactive)
  (when (yes-or-no-p (format "Download %d tracks? " (length r4-tracks--tracks)))
    (r4-download-tracks r4-tracks--tracks
                        (when (plist-get r4-tracks--source :slug) r4-tracks--tracks))))

(defun r4-tracks-browse ()
  "Open the track at point on radio4000.com."
  (interactive)
  (browse-url (r4-track-web-url (r4-tracks--at-point))))

(defun r4-tracks-browse-media ()
  "Open the media URL of the track at point."
  (interactive)
  (browse-url (r4-track-url (r4-tracks--at-point))))

(defun r4-tracks-copy-url ()
  "Copy the media URL of the track at point."
  (interactive)
  (let ((url (r4-track-url (r4-tracks--at-point))))
    (kill-new url)
    (message "Copied %s" url)))

(defun r4-tracks-open-file ()
  "Visit the downloaded file of the track at point in Dired."
  (interactive)
  (let ((file (or (r4-download-file (r4-tracks--at-point))
                  (user-error "Track not downloaded"))))
    (dired-jump nil file)))

(defun r4-tracks-search (query)
  "Search tracks matching QUERY, in the channel of the buffer if any."
  (interactive (list (read-string "Search tracks: ")))
  (let ((slug (plist-get r4-tracks--source :slug))
        (query (if (string-blank-p query) nil query)))
    (unless (or slug query) (user-error "Empty search"))
    (r4-tracks-show (list :slug slug :query query))))

(defun r4-tracks-open-channel ()
  "Open the channel of the track at point."
  (interactive)
  (r4-tracks-show (list :slug (r4-track-slug (r4-tracks--at-point)))))

(defvar-keymap r4-tracks-mode-map
  :doc "Keymap for `r4-tracks-mode'."
  "RET" #'r4-tracks-play
  "P" #'r4-tracks-play-all
  "z" #'r4-tracks-shuffle
  "SPC" #'r4-player-toggle-pause
  ">" #'r4-player-next
  "<" #'r4-player-previous
  "k" #'r4-player-stop
  "Q" #'r4-queue
  "m" #'r4-tracks-mark
  "u" #'r4-tracks-unmark
  "U" #'r4-tracks-unmark-all
  "M" #'r4-tracks-mark-all
  "d" #'r4-tracks-download
  "D" #'r4-tracks-download-all
  "L" #'r4-downloads
  "F" #'r4-tracks-open-file
  "o" #'r4-tracks-browse
  "O" #'r4-tracks-browse-media
  "w" #'r4-tracks-copy-url
  "s" #'r4-tracks-search
  "c" #'r4-tracks-open-channel)

(define-derived-mode r4-tracks-mode tabulated-list-mode "r4 tracks"
  "Major mode listing Radio4000 tracks.

The status column shows ▶ for the playing track, ✓ for downloaded
tracks and ↓ for queued downloads.  Sort by the column at point with
\\[tabulated-list-sort], or by clicking a column header.  Choose the
columns with `r4-tracks-columns'.

\\{r4-tracks-mode-map}"
  (setq r4-tracks--columns (r4-columns-valid r4-tracks-columns r4-tracks-column-specs)
        tabulated-list-format (r4-columns-list-format r4-tracks--columns
                                                      r4-tracks-column-specs)
        tabulated-list-padding 2)
  (setq-local revert-buffer-function #'r4-tracks-revert)
  (tabulated-list-init-header))

(defun r4-tracks-buffer-name (source)
  "Return the buffer name for the tracks SOURCE."
  (format "*r4 %s*" (r4-tracks--describe-source source)))

;;;###autoload
(defun r4-tracks-show (source)
  "Show the tracks of SOURCE, a plist with the keys :slug and :query."
  (with-current-buffer (get-buffer-create (r4-tracks-buffer-name source))
    (unless (derived-mode-p 'r4-tracks-mode) (r4-tracks-mode))
    (setq r4-tracks--source source)
    (revert-buffer)
    (pop-to-buffer-same-window (current-buffer))))

(add-hook 'r4-player-track-changed-hook #'r4-tracks--track-changed)
(add-hook 'r4-download-changed-functions #'r4-tracks--downloads-changed)

;;;###autoload
(defun r4-search-tracks (query)
  "Search tracks matching QUERY in all Radio4000 channels."
  (interactive (list (read-string "Search tracks: ")))
  (r4-tracks-show (list :query query)))

(provide 'r4-tracks)

;;; r4-tracks.el ends here
