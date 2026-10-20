;;; r4-download.el --- Download Radio4000 tracks with yt-dlp  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Download tracks with yt-dlp, as the best audio available, or as
;; videos, into the folders of r4-files.el.  Downloads run in a queue,
;; listed by `r4-downloads'.  Downloaded files get the artist, title
;; and channel of the track as metadata, and a text file like the r4
;; command line.  Failed downloads are skipped next time, unless
;; retried.

;;; Code:

(require 'cl-lib)
(require 'r4-models)
(require 'r4-files)

(defcustom r4-download-program "yt-dlp"
  "Program used to download tracks."
  :type 'string
  :group 'r4)

(defcustom r4-download-video nil
  "When non-nil, download videos instead of audio.
Videos and audio files of the same track can live side by side."
  :type 'boolean
  :group 'r4)

(defcustom r4-download-audio-format nil
  "Audio format of downloaded tracks.
nil keeps the best original audio stream, without re-encoding, which
gives the highest quality.  A format such as \"flac\", \"mp3\",
\"opus\" or \"m4a\" converts the audio with ffmpeg, which never
improves its quality."
  :type '(choice (const :tag "Best original audio (no re-encoding)" nil)
                 (string :tag "Format"))
  :group 'r4)

(defcustom r4-download-video-quality "bestvideo*+bestaudio/best"
  "The yt-dlp format of downloaded videos.
For example \"bestvideo*[height<=1080]+bestaudio/best\" limits them
to 1080p."
  :type 'string
  :group 'r4)

(defcustom r4-download-video-container "mkv"
  "Container of downloaded videos, \"mkv\" (any codec) or \"mp4\"."
  :type '(choice (const "mkv") (const "mp4") (const "webm"))
  :group 'r4)

(defcustom r4-download-metadata t
  "When non-nil, set the metadata of downloaded files from the track.
The title, artist (from \"Artist - Title\"), album (the channel name),
description and comment (the radio4000.com URL)."
  :type 'boolean
  :group 'r4)

(defcustom r4-download-arguments '("--embed-metadata" "--embed-thumbnail" "--no-playlist")
  "Extra arguments passed to `r4-download-program'.
For example, add \"--cookies-from-browser\" \"firefox\" to download
the best audio of YouTube Premium."
  :type '(repeat string)
  :group 'r4)

(defcustom r4-download-max-processes 2
  "Maximum number of simultaneous downloads."
  :type 'natnum
  :group 'r4)

(defconst r4-download--progress-prefix "[r4-progress]"
  "Prefix of the progress lines printed by `r4-download-program'.")

(defconst r4-download-log-buffer "*r4 download log*"
  "Name of the buffer logging the output of downloads.")

(cl-defstruct (r4-download (:constructor r4-download-create)
                           (:copier nil))
  "A download of TRACK, as KIND `audio' or `video', in the channel ALBUM.
CHANNEL-TRACKS are all the tracks of the channel, for its playlist."
  track (kind 'audio) album channel-tracks
  (status 'queued) (progress 0) process error)

(defvar r4-download--jobs nil
  "List of download jobs, oldest first.")

(defvar r4-download--pending (make-hash-table :test #'equal)
  "Hash table of the ids of queued or running tracks, to their job.")

(defvar r4-download-changed-functions nil
  "Functions called with a list of tracks whose download status changed.")

;;;; Command

(defun r4-download--literal (text)
  "Return TEXT as a literal value of yt-dlp --parse-metadata."
  (string-replace ":" "\\:" (string-replace "%" "%%" text)))

(defun r4-download--metadata-arguments (job)
  "Return the yt-dlp arguments setting the metadata of the download JOB."
  (let ((track (r4-download-track job)))
    (when r4-download-metadata
      `("--parse-metadata" ,(concat (r4-download--literal (r4-track-title track)) ":%(title)s")
        "--parse-metadata" "title:(?P<artist>.+?) - (?P<title>.+)"
        "--parse-metadata" ,(concat (r4-download--literal (r4-download-album job)) ":%(album)s")
        "--parse-metadata" ,(concat (r4-download--literal (r4-track-web-url track))
                                    ":%(meta_comment)s")
        ,@(when-let* ((description (r4-track-description track))
                      ((not (string-blank-p description))))
            (list "--parse-metadata"
                  (concat (r4-download--literal description) ":%(description)s")))))))

(defun r4-download--command (job)
  "Return the command line of the download JOB."
  (let* ((track (r4-download-track job))
         (output (concat (r4-files-tracks-directory (r4-track-slug track))
                         (string-replace "%" "%%" (r4-files-basename track))
                         ".%(ext)s")))
    `(,r4-download-program
      "--newline" "--color" "never"
      "--progress-template"
      ,(concat "download:" r4-download--progress-prefix " %(progress._percent_str)s")
      ,@(if (eq (r4-download-kind job) 'video)
            `("--format" ,r4-download-video-quality
              "--merge-output-format" ,r4-download-video-container)
          `("--format" "bestaudio/best" "--extract-audio" "--audio-quality" "0"
            ,@(when r4-download-audio-format
                (list "--audio-format" r4-download-audio-format))))
      ,@(r4-download--metadata-arguments job)
      "--output" ,output
      ,@r4-download-arguments
      "--" ,(r4-track-url track))))

;;;; Queue

(defun r4-download--active-p (job)
  "Return non-nil if JOB is queued or running."
  (memq (r4-download-status job) '(queued running)))

(defun r4-download-pending-p (track)
  "Return non-nil if TRACK is queued or being downloaded."
  (and (gethash (r4-track-id track) r4-download--pending) t))

(defun r4-download--set-status (job status)
  "Set the STATUS of JOB and notify `r4-download-changed-functions'."
  (let ((track (r4-download-track job)))
    (setf (r4-download-status job) status)
    (if (r4-download--active-p job)
        (puthash (r4-track-id track) job r4-download--pending)
      (remhash (r4-track-id track) r4-download--pending))
    (run-hook-with-args 'r4-download-changed-functions (list track))))

(defvar r4-download-refresh-function #'ignore
  "Function called when the list of downloads should be redisplayed.")

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
      (cond ((string-prefix-p r4-download--progress-prefix line)
             (when (string-match "\\([0-9.]+\\)%" line)
               (setf (r4-download-progress job) (string-to-number (match-string 1 line)))))
            (t (when (string-prefix-p "ERROR:" line)
                 (setf (r4-download-error job) line))
               (r4-download--log "%s" line))))
    (funcall r4-download-refresh-function)))

(defun r4-download--finish (job ok)
  "Finish the download JOB, successful if OK."
  (let ((track (r4-download-track job)))
    (if ok
        (progn
          (setf (r4-download-progress job) 100)
          (when-let* ((file (r4-files-track-file track (r4-download-kind job))))
            (r4-files-finish-track track file))
          (when (r4-download-channel-tracks job)
            (r4-files-write-playlist (r4-track-slug track) (r4-download-channel-tracks job)))
          (message "Downloaded: %s" (r4-track-title track)))
      (r4-files-add-failure track (or (r4-download-error job) "Download failed"))
      (message "Download failed (see %s): %s" r4-download-log-buffer (r4-track-title track)))
    (r4-download--set-status job (if ok 'done 'failed))))

(defun r4-download--sentinel (process _event)
  "Update the job of the download PROCESS when it ends."
  (unless (process-live-p process)
    (let ((job (process-get process 'r4-download)))
      (unless (eq (r4-download-status job) 'cancelled)
        (r4-download--finish job (and (eq (process-status process) 'exit)
                                      (zerop (process-exit-status process)))))
      (r4-download--start-next)
      (funcall r4-download-refresh-function))))

(defun r4-download--start (job)
  "Start the download process of JOB."
  (let ((track (r4-download-track job)))
    (make-directory (r4-files-tracks-directory (r4-track-slug track)) t)
    (r4-download--log "\n# @%s %s (%s)\n# %s" (r4-track-slug track) (r4-track-title track)
                      (r4-download-kind job) (r4-track-url track))
    (let ((process (make-process
                    :name "r4-download"
                    :command (r4-download--command job)
                    :connection-type 'pipe
                    :noquery t
                    :filter #'r4-download--filter
                    :sentinel #'r4-download--sentinel)))
      (process-put process 'r4-download job)
      (setf (r4-download-process job) process
            (r4-download-error job) nil)
      (r4-download--set-status job 'running))))

(defun r4-download--start-next ()
  "Start queued downloads, up to `r4-download-max-processes'."
  (let ((running (cl-count 'running r4-download--jobs :key #'r4-download-status)))
    (dolist (job r4-download--jobs)
      (when (and (eq (r4-download-status job) 'queued)
                 (< running r4-download-max-processes))
        (r4-download--start job)
        (cl-incf running)))))

(cl-defun r4-download-tracks (tracks &key kind channel channel-tracks retry)
  "Queue the download of TRACKS, skipping those already downloaded.
KIND is `audio' or `video', by default according to
`r4-download-video'.  CHANNEL, the `r4-channel' of the tracks, names
the album, and CHANNEL-TRACKS, all its tracks in order, make its
playlist.  Tracks whose download failed before are skipped, unless
RETRY is non-nil."
  (unless (executable-find r4-download-program)
    (user-error "Cannot find `%s', please install it" r4-download-program))
  (let ((kind (or kind (if r4-download-video 'video 'audio)))
        (failed (make-hash-table :test #'equal))
        (queued nil)
        (skipped-failures 0))
    (when channel
      (r4-files-write-channel channel (or channel-tracks tracks)))
    (dolist (slug (delete-dups (mapcar #'r4-track-slug tracks)))
      (if retry
          (r4-files-forget-failures slug)
        (dolist (id (r4-files-failed-ids slug)) (puthash id t failed))))
    (r4-files-with-index-cache
      (dolist (track tracks)
        (cond ((or (r4-files-track-file track kind) (r4-download-pending-p track)))
              ((gethash (r4-track-id track) failed) (cl-incf skipped-failures))
              (t (let ((job (r4-download-create
                             :track track :kind kind :channel-tracks channel-tracks
                             :album (if channel
                                        (r4-channel-name channel)
                                      (concat "@" (r4-track-slug track))))))
                   (puthash (r4-track-id track) job r4-download--pending)
                   (push job queued))))))
    (setq r4-download--jobs (nconc r4-download--jobs (nreverse queued)))
    (message "Queued %d track%s for download%s"
             (length queued) (if (= (length queued) 1) "" "s")
             (if (> skipped-failures 0)
                 (format ", skipped %d that failed before (retry with C-u)" skipped-failures)
               ""))
    (run-hook-with-args 'r4-download-changed-functions (mapcar #'r4-download-track queued))
    (r4-download--start-next)
    (funcall r4-download-refresh-function)))

(defun r4-download-cancel (job)
  "Cancel the download JOB."
  (when (r4-download--active-p job)
    (r4-download--set-status job 'cancelled)
    (when-let* ((process (r4-download-process job)))
      (delete-process process))))

(defun r4-download-retry (job)
  "Queue the download JOB again."
  (unless (r4-download--active-p job)
    (setf (r4-download-progress job) 0)
    (r4-download--set-status job 'queued)
    (r4-download--start-next)))

(defun r4-download-cancel-all ()
  "Cancel all queued and running downloads."
  (interactive)
  (mapc #'r4-download-cancel r4-download--jobs)
  (funcall r4-download-refresh-function)
  (message "Cancelled all downloads"))

(provide 'r4-download)

;;; r4-download.el ends here
