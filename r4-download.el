;;; r4-download.el --- Download Radio4000 tracks with yt-dlp  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Download tracks as high quality audio files into
;; `r4-download-directory', one folder per channel, using yt-dlp.
;; Downloads run asynchronously in a queue; `r4-downloads' lists them.
;; Downloaded tracks are played from disk by the player.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'r4-models)

(defcustom r4-download-directory "~/Music/radio4000/"
  "Directory where tracks are downloaded, in one sub-directory per channel."
  :type 'directory
  :group 'r4)

(defcustom r4-download-program "yt-dlp"
  "Program used to download tracks."
  :type 'string
  :group 'r4)

(defcustom r4-download-audio-format nil
  "Audio format of downloaded tracks.
nil keeps the best original audio stream, without re-encoding, which
gives the highest quality.  A format such as \"flac\", \"mp3\",
\"opus\" or \"m4a\" converts the audio with ffmpeg."
  :type '(choice (const :tag "Best original audio (no re-encoding)" nil)
                 (string :tag "Format"))
  :group 'r4)

(defcustom r4-download-arguments '("--embed-metadata" "--embed-thumbnail" "--no-playlist")
  "Extra arguments passed to `r4-download-program'."
  :type '(repeat string)
  :group 'r4)

(defcustom r4-download-max-processes 2
  "Maximum number of simultaneous downloads."
  :type 'natnum
  :group 'r4)

(defcustom r4-download-write-playlist t
  "When non-nil, write a CHANNEL.m3u playlist in each channel directory."
  :type 'boolean
  :group 'r4)

(defconst r4-download--audio-extensions
  '("opus" "m4a" "mp3" "flac" "ogg" "oga" "wav" "webm" "aac" "mka" "aiff" "mp4")
  "File extensions considered downloaded audio.")

(defconst r4-download--progress-prefix "[r4-progress]"
  "Prefix of the progress lines printed by `r4-download-program'.")

(cl-defstruct (r4-download (:constructor r4-download-create)
                           (:copier nil))
  "A download job for TRACK."
  track (status 'queued) (progress 0) process)

(defvar r4-download--jobs nil
  "List of download jobs, oldest first.")

(defvar r4-download--playlists (make-hash-table :test #'equal)
  "Hash table of channel slug to its ordered tracks, used for playlists.")

(defvar r4-download-finished-hook nil
  "Hook run with the finished `r4-download' job as argument.")

(defconst r4-download-log-buffer "*r4 download log*"
  "Name of the buffer logging the output of downloads.")

;;;; Files

(defun r4-download-channel-directory (slug)
  "Return the download directory of the channel SLUG."
  (file-name-as-directory (expand-file-name slug r4-download-directory)))

(defun r4-download--sanitize (name)
  "Return NAME usable as a file name on common file systems."
  (let ((clean (string-trim
                (replace-regexp-in-string "[/\\:*?\"<>|[:cntrl:]]+" "_" (or name "")))))
    ;; Limit the length in bytes, the file name limit of most file
    ;; systems.  `string-limit' with a coding system hangs on some
    ;; scripts, such as Thai or Persian, in Emacs 31.
    (while (> (string-bytes clean) 180)
      (setq clean (substring clean 0 -1)))
    (if (string-empty-p clean) "untitled" clean)))

(defun r4-download-basename (track)
  "Return the file name of TRACK, without directory nor extension."
  (format "%s [%s]"
          (r4-download--sanitize (r4-track-title track))
          (or (r4-track-media-id track)
              (substring (r4-track-id track) 0 8))))

(defun r4-download-index (slug)
  "Return a hash table of the downloaded files of the channel SLUG.
Keys are file names without extension, values are absolute file names."
  (let ((index (make-hash-table :test #'equal))
        (directory (r4-download-channel-directory slug)))
    (when (file-directory-p directory)
      (dolist (file (directory-files
                     directory t
                     (concat "\\." (regexp-opt r4-download--audio-extensions) "\\'")
                     t))
        (puthash (file-name-base file) file index)))
    index))

(defun r4-download-file (track &optional index)
  "Return the downloaded file of TRACK, or nil.
INDEX is the result of `r4-download-index' for the track channel."
  (gethash (r4-download-basename track)
           (or index (r4-download-index (r4-track-slug track)))))

(defun r4-download-write-playlist (slug)
  "Write the M3U playlist of the downloaded tracks of the channel SLUG."
  (when-let* ((tracks (gethash slug r4-download--playlists))
              (index (r4-download-index slug)))
    (with-temp-file (expand-file-name (concat slug ".m3u")
                                      (r4-download-channel-directory slug))
      (insert "#EXTM3U\n")
      (dolist (track tracks)
        (when-let* ((file (r4-download-file track index)))
          (insert (format "#EXTINF:%d,%s\n%s\n"
                          (or (r4-track-duration track) -1)
                          (r4-one-line (r4-track-title track))
                          (file-name-nondirectory file))))))))

;;;; Queue

(defun r4-download--command (track)
  "Return the command line downloading TRACK."
  (let ((output (concat (r4-download-channel-directory (r4-track-slug track))
                        (string-replace "%" "%%" (r4-download-basename track))
                        ".%(ext)s")))
    `(,r4-download-program
      "--newline" "--color" "never"
      "--progress-template"
      ,(concat "download:" r4-download--progress-prefix " %(progress._percent_str)s")
      "--format" "bestaudio/best" "--extract-audio" "--audio-quality" "0"
      ,@(when r4-download-audio-format
          (list "--audio-format" r4-download-audio-format))
      "--output" ,output
      ,@r4-download-arguments
      "--" ,(r4-track-url track))))

(defun r4-download--active-p (job)
  "Return non-nil if JOB is queued or running."
  (memq (r4-download-status job) '(queued running)))

(defun r4-download--job (track)
  "Return the queued or running job of TRACK, or nil."
  (cl-find-if (lambda (job)
                (and (r4-download--active-p job)
                     (equal (r4-track-id (r4-download-track job)) (r4-track-id track))))
              r4-download--jobs))

(defun r4-download-pending-p (track)
  "Return non-nil if TRACK is queued or being downloaded."
  (and (r4-download--job track) t))

(defun r4-download--log (format-string &rest args)
  "Append FORMAT-STRING formatted with ARGS to the download log."
  (with-current-buffer (get-buffer-create r4-download-log-buffer)
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (apply #'format format-string args) "\n")))))

(defun r4-download--filter (process output)
  "Log OUTPUT of the download PROCESS and track its progress."
  (let* ((job (process-get process 'r4-download))
         (lines (split-string (concat (process-get process 'r4-pending) output) "\n")))
    (process-put process 'r4-pending (car (last lines)))
    (dolist (line (butlast lines))
      (if (string-prefix-p r4-download--progress-prefix line)
          (when (string-match "\\([0-9.]+\\)%" line)
            (setf (r4-download-progress job) (string-to-number (match-string 1 line))))
        (r4-download--log "%s" line)))
    (r4-download--refresh)))

(defun r4-download--sentinel (process _event)
  "Update the job of the download PROCESS when it ends."
  (unless (process-live-p process)
    (let* ((job (process-get process 'r4-download))
           (track (r4-download-track job))
           (ok (and (eq (process-status process) 'exit)
                    (zerop (process-exit-status process)))))
      (unless (eq (r4-download-status job) 'cancelled)
        (setf (r4-download-status job) (if ok 'done 'failed))
        (when ok (setf (r4-download-progress job) 100))
        (message (if ok "Downloaded: %s" "Download failed (see %s): %s")
                 (if ok (r4-track-title track) r4-download-log-buffer)
                 (r4-track-title track)))
      (when (and ok r4-download-write-playlist)
        (r4-download-write-playlist (r4-track-slug track)))
      (run-hook-with-args 'r4-download-finished-hook job)
      (r4-download--start-next)
      (r4-download--refresh))))

(defun r4-download--start (job)
  "Start the download process of JOB."
  (let ((track (r4-download-track job)))
    (make-directory (r4-download-channel-directory (r4-track-slug track)) t)
    (r4-download--log "\n# @%s %s\n# %s" (r4-track-slug track) (r4-track-title track)
                      (r4-track-url track))
    (let ((process (make-process
                    :name "r4-download"
                    :command (r4-download--command track)
                    :connection-type 'pipe
                    :noquery t
                    :filter #'r4-download--filter
                    :sentinel #'r4-download--sentinel)))
      (process-put process 'r4-download job)
      (setf (r4-download-process job) process
            (r4-download-status job) 'running))))

(defun r4-download--start-next ()
  "Start queued downloads, up to `r4-download-max-processes'."
  (let ((running (cl-count 'running r4-download--jobs :key #'r4-download-status)))
    (dolist (job r4-download--jobs)
      (when (and (eq (r4-download-status job) 'queued)
                 (< running r4-download-max-processes))
        (r4-download--start job)
        (cl-incf running)))))

(defun r4-download-tracks (tracks &optional channel-tracks)
  "Queue the download of TRACKS, skipping those already downloaded.
CHANNEL-TRACKS, all the tracks of one channel in order, is used to write
the channel playlist."
  (unless (executable-find r4-download-program)
    (user-error "Cannot find `%s', please install it" r4-download-program))
  (when channel-tracks
    (puthash (r4-track-slug (car channel-tracks)) channel-tracks r4-download--playlists))
  (let ((indexes (make-hash-table :test #'equal))
        (count 0))
    (dolist (track tracks)
      (let* ((slug (r4-track-slug track))
             (index (or (gethash slug indexes)
                        (puthash slug (r4-download-index slug) indexes))))
        (unless (or (r4-download-file track index) (r4-download-pending-p track))
          (setq r4-download--jobs
                (nconc r4-download--jobs (list (r4-download-create :track track))))
          (cl-incf count))))
    (message "Queued %d track%s for download (%d already downloaded or queued)"
             count (if (= count 1) "" "s") (- (length tracks) count))
    (r4-download--start-next)
    (r4-download--refresh)))

(defun r4-download-cancel (job)
  "Cancel the download JOB."
  (when (r4-download--active-p job)
    (setf (r4-download-status job) 'cancelled)
    (when-let* ((process (r4-download-process job)))
      (delete-process process))))

(defun r4-download-cancel-all ()
  "Cancel all queued and running downloads."
  (interactive)
  (mapc #'r4-download-cancel r4-download--jobs)
  (r4-download--refresh)
  (message "Cancelled all downloads"))

(defun r4-download-open-directory (&optional slug)
  "Open the download directory, or the one of the channel SLUG, in Dired."
  (interactive)
  (let ((directory (if slug
                       (r4-download-channel-directory slug)
                     (file-name-as-directory (expand-file-name r4-download-directory)))))
    (make-directory directory t)
    (dired directory)))

;;;; Downloads list

(defconst r4-downloads-buffer "*r4 downloads*"
  "Name of the buffer listing downloads.")

(defvar r4-download--refresh-timer nil
  "Timer refreshing the downloads list buffer.")

(defun r4-download--refresh ()
  "Refresh the downloads list buffer soon, if it exists."
  (when (and (get-buffer r4-downloads-buffer) (not r4-download--refresh-timer))
    (setq r4-download--refresh-timer
          (run-with-timer 0.5 nil #'r4-download--refresh-now))))

(defun r4-download--refresh-now ()
  "Refresh the downloads list buffer."
  (setq r4-download--refresh-timer nil)
  (when-let* ((buffer (get-buffer r4-downloads-buffer)))
    (with-current-buffer buffer
      (revert-buffer))))

(defun r4-downloads--entries ()
  "Return the `tabulated-list-entries' of the downloads list."
  (mapcar (lambda (job)
            (let ((track (r4-download-track job)))
              (list job
                    (vector (symbol-name (r4-download-status job))
                            (format "%5.1f%%" (r4-download-progress job))
                            (concat "@" (r4-track-slug track))
                            (r4-one-line (r4-track-title track))))))
          (reverse r4-download--jobs)))

(defun r4-downloads-cancel ()
  "Cancel the download at point."
  (interactive)
  (if-let* ((job (tabulated-list-get-id)))
      (progn (r4-download-cancel job) (revert-buffer))
    (user-error "No download at point")))

(defun r4-downloads-retry ()
  "Queue again the download at point."
  (interactive)
  (let ((job (or (tabulated-list-get-id) (user-error "No download at point"))))
    (when (r4-download--active-p job)
      (user-error "Already downloading"))
    (setf (r4-download-status job) 'queued
          (r4-download-progress job) 0)
    (r4-download--start-next)
    (revert-buffer)))

(defun r4-downloads-clear ()
  "Remove finished, failed and cancelled downloads from the list."
  (interactive)
  (setq r4-download--jobs (cl-remove-if-not #'r4-download--active-p r4-download--jobs))
  (revert-buffer))

(defun r4-downloads-show-log ()
  "Show the download log."
  (interactive)
  (display-buffer (get-buffer-create r4-download-log-buffer)))

(defvar-keymap r4-downloads-mode-map
  :doc "Keymap for `r4-downloads-mode'."
  "k" #'r4-downloads-cancel
  "K" #'r4-download-cancel-all
  "r" #'r4-downloads-retry
  "c" #'r4-downloads-clear
  "l" #'r4-downloads-show-log
  "o" #'r4-download-open-directory)

(define-derived-mode r4-downloads-mode tabulated-list-mode "r4 downloads"
  "Major mode listing Radio4000 downloads.

\\{r4-downloads-mode-map}"
  (setq tabulated-list-format [("Status" 10 t)
                               ("Progress" 9 t :right-align t)
                               ("Channel" 20 t)
                               ("Title" 0 t)]
        tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook
            (lambda () (setq tabulated-list-entries (r4-downloads--entries)))
            nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun r4-downloads ()
  "List Radio4000 downloads."
  (interactive)
  (with-current-buffer (get-buffer-create r4-downloads-buffer)
    (r4-downloads-mode)
    (revert-buffer)
    (pop-to-buffer (current-buffer))))

(provide 'r4-download)

;;; r4-download.el ends here
