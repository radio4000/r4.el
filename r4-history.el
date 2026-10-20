;;; r4-history.el --- Radio4000 play history  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The tracks played, newest first, kept on this computer only in
;; `r4-history-file', like the play history of radio4000.com.

;;; Code:

(require 'r4-models)
(require 'r4-columns)
(require 'r4-player)
(require 'r4-tracks)

(defcustom r4-history-file (locate-user-emacs-file "r4-history.eld")
  "File where the played tracks are saved, or nil to keep no history."
  :type '(choice (file :tag "File") (const :tag "No history" nil))
  :group 'r4)

(defcustom r4-history-size 1000
  "Maximum number of played tracks kept in the history."
  :type 'natnum
  :group 'r4)

(defvar r4-history--entries nil
  "List of the played tracks, newest first, as (TIME . TRACK).
TIME is an ISO 8601 timestamp.")

(defvar r4-history--loaded nil
  "Non-nil once `r4-history-file' is loaded.")

(defvar r4-history--times (make-hash-table :test #'eq :weakness 'key)
  "Hash table of the tracks shown in the history, to when they played.")

(defun r4-history--load ()
  "Load `r4-history-file' once."
  (unless r4-history--loaded
    (setq r4-history--loaded t
          r4-history--entries
          (and r4-history-file (file-readable-p r4-history-file)
               (ignore-errors
                 (with-temp-buffer
                   (insert-file-contents r4-history-file)
                   (read (current-buffer))))))))

(defun r4-history--save ()
  "Save the history in `r4-history-file'."
  (when r4-history-file
    (let ((print-length nil) (print-level nil))
      (with-temp-file r4-history-file
        (prin1 r4-history--entries (current-buffer))))))

(defun r4-history--record ()
  "Add the track starting to play to the history."
  (when-let* ((r4-history-file)
              (track (and (r4-player-live-p) (r4-player-current))))
    (r4-history--load)
    (unless (eq track (cdar r4-history--entries))
      (push (cons (format-time-string "%FT%T%z") track) r4-history--entries)
      (setq r4-history--entries (seq-take r4-history--entries r4-history-size))
      (r4-history--save))))

(add-hook 'r4-player-track-changed-hook #'r4-history--record)

(add-to-list 'r4-tracks-column-specs
             `(played . ,(r4-column-create
                          :name "Played" :width 16
                          :key (lambda (track) (gethash track r4-history--times))
                          :format (lambda (time)
                                    (if time
                                        (format-time-string "%F %R" (date-to-time time))
                                      "")))))

(defun r4-history--fetch (source callback)
  "Call CALLBACK with the played tracks, if SOURCE is the history."
  (when (plist-get source :history)
    (r4-history--load)
    (funcall callback
             (mapcar (lambda (entry)
                       (puthash (cdr entry) (car entry) r4-history--times)
                       (cdr entry))
                     r4-history--entries))
    t))

(add-hook 'r4-tracks-fetch-functions #'r4-history--fetch)

;;;###autoload
(defun r4-history ()
  "List the played tracks, newest first."
  (interactive)
  (r4-tracks-show '(:history t :name "history"
                    :columns (status played channel title duration))))

(defun r4-history-clear ()
  "Forget all the played tracks."
  (interactive)
  (when (yes-or-no-p "Forget all the played tracks? ")
    (setq r4-history--entries nil
          r4-history--loaded t)
    (r4-history--save)
    (message "Cleared the history")))

(provide 'r4-history)

;;; r4-history.el ends here
