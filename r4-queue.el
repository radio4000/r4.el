;;; r4-queue.el --- The Radio4000 play queue  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The list of the tracks in the play queue, see r4-player.el.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'r4-models)
(require 'r4-player)

(defconst r4-queue-buffer "*r4 queue*"
  "Name of the buffer listing the play queue.")

(defun r4-queue--entries ()
  "Return the `tabulated-list-entries' of the play queue."
  (cl-loop for track across r4-player-queue
           for index from 0
           collect (list index
                         (vector (if (eql index r4-player-index) "▶" "")
                                 (concat "@" (r4-track-slug track))
                                 (r4-one-line (r4-track-title track))
                                 (r4-format-duration (r4-track-duration track))))))

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
  "W" #'r4-queue-copy-web-url)

(define-derived-mode r4-queue-mode tabulated-list-mode "r4 queue"
  "Major mode listing the Radio4000 play queue.

\\{r4-queue-mode-map}"
  (setq tabulated-list-format [(" " 1 nil) ("Channel" 20 t) ("Title" 60 t)
                               ("Time" 8 nil :right-align t)]
        tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook
            (lambda () (setq tabulated-list-entries (r4-queue--entries)))
            nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun r4-queue ()
  "Show the Radio4000 play queue."
  (interactive)
  (with-current-buffer (get-buffer-create r4-queue-buffer)
    (r4-queue-mode)
    (revert-buffer)
    (when r4-player-index
      (goto-char (point-min))
      (forward-line r4-player-index))
    (pop-to-buffer (current-buffer))))

(provide 'r4-queue)

;;; r4-queue.el ends here
