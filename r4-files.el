;;; r4-files.el --- Radio4000 files on disk  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The folders and files of downloaded channels, the same as the ones
;; of the r4 command line (`r4 download'), so both share downloads and
;; radio4000.com can import them:
;;
;;   ~/Music/radio4000/SLUG/
;;   ├── SLUG.txt                 the channel
;;   ├── SLUG.json                the local channel, see r4-local.el
;;   ├── SLUG.m3u                 playlist of the downloaded files
;;   ├── tracks.m3u               playlist of the track URLs
;;   ├── image.url                the channel image
;;   ├── failures.jsonl           the downloads that failed
;;   └── tracks/
;;       ├── Title [youtube-id].opus
;;       └── Title [youtube-id].txt

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ucs-normalize)
(require 'r4-models)

(defun r4-files--cli-downloads-directory ()
  "Return the downloads directory configured for the r4 command line, or nil."
  (ignore-errors
    (let ((config (expand-file-name "radio4000/config.json"
                                    (or (getenv "XDG_CONFIG_HOME") "~/.config"))))
      (when (file-readable-p config)
        (with-temp-buffer
          (insert-file-contents config)
          (let ((directory (alist-get 'downloadsDir
                                      (json-parse-buffer :object-type 'alist
                                                         :null-object nil))))
            (and (stringp directory) (file-name-as-directory directory))))))))

(defcustom r4-download-directory
  (or (r4-files--cli-downloads-directory) "~/Music/radio4000/")
  "Directory where channels are downloaded, one sub-directory per channel.
Defaults to the downloads directory of the r4 command line, when
configured, or else ~/Music/radio4000/."
  :type 'directory
  :group 'r4)

(defconst r4-files-audio-extensions
  '("opus" "m4a" "mp3" "flac" "ogg" "oga" "wav" "aac" "aiff" "alac")
  "Extensions of downloaded audio files.")

(defconst r4-files-video-extensions
  '("mkv" "mp4" "webm" "mov" "avi")
  "Extensions of downloaded video files.")

;;;; Names

(defun r4-files-channel-directory (slug)
  "Return the download directory of the channel SLUG."
  (file-name-as-directory (expand-file-name slug r4-download-directory)))

(defun r4-files-tracks-directory (slug)
  "Return the directory of the downloaded tracks of the channel SLUG."
  (file-name-as-directory (expand-file-name "tracks" (r4-files-channel-directory slug))))

(defun r4-files--strip-trailing (name)
  "Return NAME without trailing dots and spaces."
  (replace-regexp-in-string "[. ]+\\'" "" name))

(defun r4-files--control-char-p (char)
  "Return non-nil if CHAR is a control or format character, except ZWJ."
  (and (/= char #x200D)
       (or (<= #xFFF0 char #xFFFF)
           (memq (get-char-code-property char 'general-category) '(Cc Cf Zl Zp)))))

(defun r4-files--utf16-length (string)
  "Return the length of STRING in UTF-16 code units, like JavaScript."
  (cl-loop for char across string sum (if (> char #xFFFF) 2 1)))

(defun r4-files--truncate (name max)
  "Return NAME truncated to MAX UTF-16 code units, like filenamify.
A dot starts an extension that is kept."
  (if (<= (r4-files--utf16-length name) max)
      name
    (let* ((dot (cl-position ?. name :from-end t))
           (base (if dot (substring name 0 dot) name))
           (extension (if dot (substring name dot) ""))
           (budget (max 0 (- max (r4-files--utf16-length extension))))
           (end 0) (used 0))
      (while (and (< end (length base))
                  (<= (+ used (if (> (aref base end) #xFFFF) 2 1)) budget))
        (cl-incf used (if (> (aref base end) #xFFFF) 2 1))
        (cl-incf end))
      (concat (string-trim-right (substring base 0 end) " +") extension))))

(defun r4-files-sanitize (name &optional max)
  "Return NAME usable as a file name, like filenamify in the r4 command line.
Reserved and control characters become !, and NAME is cut to MAX
characters, 180 by default."
  (let ((name (ucs-normalize-NFC-string (or name ""))))
    (setq name (replace-regexp-in-string
                "[\t\n\r\u00A0\u1680\u2000-\u200A\u202F\u205F\u3000]+" " " name)
          name (replace-regexp-in-string
                "\\([<>:\"/\\\\|?*[:cntrl:]]\\)\\{2,\\}" "\\1" name)
          name (r4-files--strip-trailing name)
          name (replace-regexp-in-string "\\`\\.+\\(?:\\\\\\|/\\)\\|\\`\\.+\\'" "!" name)
          name (replace-regexp-in-string "[<>:\"/\\\\|?*[:cntrl:]]" "!" name t t)
          name (concat (mapcar (lambda (char) (if (r4-files--control-char-p char) ?! char))
                               name))
          name (r4-files--strip-trailing name))
    (when (string-empty-p name) (setq name "!"))
    (setq name (r4-files--strip-trailing (r4-files--truncate name (or max 180))))
    (if (string-match-p "\\`\\(?:con\\|prn\\|aux\\|nul\\|com[0-9]\\|lpt[0-9]\\)\\'"
                        (downcase name))
        (concat name "!")
      name)))

(defun r4-files-basename (track)
  "Return the file name of TRACK, without directory nor extension.
It is its title, followed by its YouTube id, like the r4 command line."
  (let ((title (r4-files-sanitize (r4-track-title track)))
        (media-id (and (equal (r4-track-provider track) "youtube")
                       (r4-track-media-id track))))
    (if media-id (format "%s [%s]" title media-id) title)))

;;;; Index of the downloaded files

(defvar r4-files--indexes (make-hash-table :test #'equal)
  "Cache of `r4-files-index', by channel slug.
Values are a cons of the modification time of the tracks directory and
its index.")

(defvar r4-files--index-cache nil
  "Cache of `r4-files-index' by slug.
Bound by `r4-files-with-index-cache'.")

(defmacro r4-files-with-index-cache (&rest body)
  "Run BODY, checking the download directories at most once per channel.
Use it around code calling `r4-files-track-file' for many tracks."
  (declare (indent 0) (debug t))
  `(let ((r4-files--index-cache (make-hash-table :test #'equal)))
     ,@body))

(defun r4-files--media-regexp ()
  "Return the regexp matching downloaded media files."
  (concat "\\." (regexp-opt (append r4-files-audio-extensions r4-files-video-extensions))
          "\\'"))

(defun r4-files--build-index (slug)
  "Return the index of the downloaded files of SLUG, see `r4-files-index'."
  (let* ((directory (r4-files-tracks-directory slug))
         (modified (file-attribute-modification-time (file-attributes directory)))
         (cached (gethash slug r4-files--indexes)))
    (if (and cached (equal (car cached) modified))
        (cdr cached)
      (let ((index (make-hash-table :test #'equal)))
        (when modified
          (dolist (file (directory-files directory t (r4-files--media-regexp) t))
            (push file (gethash (file-name-base file) index))))
        (puthash slug (cons modified index) r4-files--indexes)
        index))))

(defun r4-files-index (slug)
  "Return a hash table of the downloaded files of the channel SLUG.
Keys are file names without extension, values are lists of absolute
file names.  The index is cached until the tracks directory changes."
  (if r4-files--index-cache
      (or (gethash slug r4-files--index-cache)
          (puthash slug (r4-files--build-index slug) r4-files--index-cache))
    (r4-files--build-index slug)))

(defun r4-files-video-p (file)
  "Return non-nil if FILE is a video file."
  (member (downcase (or (file-name-extension file) "")) r4-files-video-extensions))

(defun r4-files-track-files (track)
  "Return the downloaded files of TRACK, audio or video."
  (gethash (r4-files-basename track) (r4-files-index (r4-track-slug track))))

(defun r4-files-track-file (track &optional kind)
  "Return a downloaded file of TRACK, or nil.
KIND is `audio' or `video' to only return that kind of file, or nil
for any, preferring audio."
  (let ((files (r4-files-track-files track)))
    (pcase kind
      ('audio (cl-find-if-not #'r4-files-video-p files))
      ('video (cl-find-if #'r4-files-video-p files))
      (_ (or (cl-find-if-not #'r4-files-video-p files) (car files))))))

;;;; Channel and track files

(defun r4-files--date (timestamp)
  "Return the local date of the ISO TIMESTAMP, or Unknown."
  (if (and (stringp timestamp) (not (string-empty-p timestamp)))
      (format-time-string "%x" (date-to-time timestamp))
    "Unknown"))

(defun r4-files--present-p (value)
  "Return non-nil if VALUE is a non-empty string."
  (and (stringp value) (not (string-empty-p value))))

(defun r4-files-channel-text (channel)
  "Return CHANNEL as text, like `r4 channel view --format text'."
  (let ((title (or (r4-channel-name channel) "Untitled Channel")))
    (concat title "\n" (make-string (length title) ?=) "\n\n"
            (or (r4-channel-description channel) "") "\n\n"
            "Info:\n"
            "  ID: " (or (r4-channel-id channel) "N/A") "\n"
            "  Slug: " (r4-channel-slug channel) "\n"
            "  Created: " (r4-files--date (r4-channel-created-at channel)) "\n"
            "  Updated: " (r4-files--date (r4-channel-updated-at channel)) "\n"
            (if (r4-files--present-p (r4-channel-url channel))
                (format "  Website: %s\n" (r4-channel-url channel))
              "")
            (if (r4-files--present-p (r4-channel-image channel))
                (format "  Image: %s\n" (r4-channel-image channel))
              "")
            (format "  Tracks: %s\n" (or (r4-channel-track-count channel) 0)))))

(defun r4-files-track-text (track)
  "Return TRACK as text, like `r4 track list --format text'."
  (format "%s\n%s\n  %s" (r4-track-title track) (or (r4-track-description track) "")
          (r4-track-url track)))

(defun r4-files--write (file text)
  "Write TEXT to FILE, in UTF-8."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)))

(defun r4-files-write-channel (channel tracks)
  "Write the files of CHANNEL with its TRACKS, like `r4 download'.
That is SLUG.txt, image.url and the tracks.m3u playlist of track URLs."
  (let ((directory (r4-files-channel-directory (r4-channel-slug channel))))
    (make-directory (r4-files-tracks-directory (r4-channel-slug channel)) t)
    (r4-files--write (expand-file-name (concat (r4-channel-slug channel) ".txt") directory)
                     (concat (r4-files-channel-text channel)
                             (format "\nStats:\n  Tracks: %d\n" (length tracks))))
    (r4-files--write (expand-file-name "image.url" directory)
                     (if-let* ((url (r4-channel-image-url channel)))
                         (concat url "\n")
                       ""))
    (r4-files--write (expand-file-name "tracks.m3u" directory)
                     (concat "#EXTM3U\n"
                             (mapconcat (lambda (track)
                                          (format "#EXTINF:-1,%s\n%s\n"
                                                  (r4-one-line (r4-track-title track))
                                                  (r4-track-url track)))
                                        tracks "")))))

(defun r4-files-write-playlist (slug tracks)
  "Write SLUG.m3u, the playlist of the downloaded files of TRACKS, in order."
  (r4-files-with-index-cache
    (r4-files--write
     (expand-file-name (concat slug ".m3u") (r4-files-channel-directory slug))
     (concat "#EXTM3U\n"
             (mapconcat (lambda (track)
                          (if-let* ((file (r4-files-track-file track)))
                              (format "#EXTINF:%d,%s\ntracks/%s\n"
                                      (or (r4-track-duration track) -1)
                                      (r4-one-line (r4-track-title track))
                                      (file-name-nondirectory file))
                            ""))
                        tracks "")))))

(defconst r4-files-image-extensions '("webp" "png" "jpg" "jpeg")
  "Extensions of the thumbnails downloaded with tracks.")

(defun r4-files--delete-thumbnails (file)
  "Delete the thumbnails left next to the media FILE, if any.
yt-dlp leaves them when it cannot embed them."
  (dolist (extension r4-files-image-extensions)
    (let ((thumbnail (concat (file-name-sans-extension file) "." extension)))
      (when (file-exists-p thumbnail)
        (delete-file thumbnail)))))

(defun r4-files-finish-track (track file)
  "Write the text file of the downloaded TRACK in FILE, and date them.
The files get the date the track was added to the channel.  Leftover
thumbnails are deleted."
  (let ((text-file (concat (file-name-sans-extension file) ".txt"))
        (time (and (r4-track-created-at track) (date-to-time (r4-track-created-at track)))))
    (r4-files--delete-thumbnails file)
    (r4-files--write text-file (r4-files-track-text track))
    (when time
      (dolist (path (list file text-file))
        (ignore-errors (set-file-times path time))))))

;;;; Migration

(defun r4-files--flat-media (slug)
  "Return the media files of SLUG outside of its tracks directory.
Older versions of r4.el downloaded them there."
  (let ((directory (r4-files-channel-directory slug)))
    (when (file-directory-p directory)
      (directory-files directory t (r4-files--media-regexp) t))))

(defun r4-files-migrate (&optional slugs)
  "Move the downloads of older r4.el versions into the tracks directories.
SLUGS are the channels to migrate, all by default.  Media files move
to SLUG/tracks/, unless a file with the same name is there, and the
thumbnails left next to them are deleted."
  (interactive)
  (let* ((root (expand-file-name r4-download-directory))
         (slugs (or slugs
                    (and (file-directory-p root)
                         (cl-remove-if-not
                          (lambda (slug) (file-directory-p (expand-file-name slug root)))
                          (directory-files root nil "\\`[^.]")))))
         (files (mapcan (lambda (slug) (mapcar (lambda (file) (cons slug file))
                                                (r4-files--flat-media slug)))
                        slugs))
         (moved 0) (kept 0))
    (cond
     ((null files) (message "Nothing to migrate in %s" root))
     ((yes-or-no-p (format "Move %d downloaded file%s of %d channel%s into their tracks directory? "
                           (length files) (if (cdr files) "s" "")
                           (length (delete-dups (mapcar #'car files)))
                           (if (cdr (delete-dups (mapcar #'car files))) "s" "")))
      (pcase-dolist (`(,slug . ,file) files)
        (let ((target (expand-file-name (file-name-nondirectory file)
                                        (r4-files-tracks-directory slug))))
          (make-directory (file-name-directory target) t)
          (if (file-exists-p target)
              (cl-incf kept)
            (rename-file file target)
            (r4-files--delete-thumbnails file)
            (cl-incf moved))))
      (message "Moved %d file%s%s" moved (if (= moved 1) "" "s")
               (if (> kept 0) (format ", left %d already in tracks/" kept) ""))))))

;;;; Failures

(defun r4-files--failures-file (slug)
  "Return the file listing the failed downloads of the channel SLUG."
  (expand-file-name "failures.jsonl" (r4-files-channel-directory slug)))

(defun r4-files-failed-ids (slug)
  "Return the ids of the tracks of SLUG whose download failed."
  (let ((file (r4-files--failures-file slug)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (cl-loop for line in (split-string (buffer-string) "\n" t)
                 for failure = (ignore-errors
                                 (json-parse-string line :object-type 'alist
                                                    :null-object nil))
                 for id = (alist-get 'id (alist-get 'track failure))
                 when id collect id)))))

(defvar r4-files--failures (make-hash-table :test #'equal)
  "Cache of the failed track ids by channel slug.
Values are a cons of the modification time of the failures file and a
hash table of the ids.")

(defun r4-files-failed-p (track)
  "Return non-nil if the download of TRACK failed before."
  (gethash (r4-track-id track)
           (if r4-files--index-cache
               (let ((key (cons 'failed (r4-track-slug track))))
                 (or (gethash key r4-files--index-cache)
                     (puthash key (r4-files--failed-ids (r4-track-slug track))
                              r4-files--index-cache)))
             (r4-files--failed-ids (r4-track-slug track)))))

(defun r4-files--failed-ids (slug)
  "Return a hash table of the failed track ids of SLUG, cached."
  (let* ((file (r4-files--failures-file slug))
         (modified (file-attribute-modification-time (file-attributes file)))
         (cached (gethash slug r4-files--failures)))
    (unless (and cached (equal (car cached) modified))
      (let ((ids (make-hash-table :test #'equal)))
        (when modified
          (dolist (id (r4-files-failed-ids slug)) (puthash id t ids)))
        (setq cached (puthash slug (cons modified ids) r4-files--failures))))
    (cdr cached)))

(defun r4-files-add-failure (track error)
  "Add TRACK to the failed downloads of its channel, because of ERROR."
  (let ((file (r4-files--failures-file (r4-track-slug track))))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region
       (concat (json-serialize
                `((timestamp . ,(format-time-string "%FT%T.%3NZ" nil t))
                  (track . ((id . ,(r4-track-id track))
                            (title . ,(or (r4-track-title track) ""))
                            (url . ,(or (r4-track-url track) ""))
                            (media_id . ,(or (r4-track-media-id track) :null))))
                  (error . ,(or error ""))))
               "\n")
       nil file 'append 'silent))))

(defun r4-files-forget-failures (slug)
  "Forget the failed downloads of the channel SLUG, to retry them."
  (let ((file (r4-files--failures-file slug)))
    (when (file-exists-p file) (delete-file file))))

(defun r4-files-open-directory (&optional slug)
  "Open the download directory, or the one of the channel SLUG, in Dired."
  (interactive)
  (let ((directory (if slug
                       (r4-files-channel-directory slug)
                     (file-name-as-directory (expand-file-name r4-download-directory)))))
    (make-directory directory t)
    (dired directory)))

(provide 'r4-files)

;;; r4-files.el ends here
