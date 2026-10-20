;;; r4-downloads.el --- List Radio4000 downloads  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The list of downloads, with their progress, see r4-download.el.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'r4-models)
(require 'r4-files)
(require 'r4-download)

(defconst r4-downloads-buffer "*r4 downloads*"
  "Name of the buffer listing downloads.")

(defvar r4-downloads--refresh-timer nil
  "Timer refreshing the downloads list buffer.")

(defun r4-downloads--refresh ()
  "Refresh the downloads list buffer soon, if it exists."
  (when (and (get-buffer r4-downloads-buffer) (not r4-downloads--refresh-timer))
    (setq r4-downloads--refresh-timer
          (run-with-timer 0.5 nil #'r4-downloads--refresh-now))))

(setq r4-download-refresh-function #'r4-downloads--refresh)

(defun r4-downloads--refresh-now ()
  "Refresh the downloads list buffer."
  (setq r4-downloads--refresh-timer nil)
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
                            (symbol-name (r4-download-kind job))
                            (concat "@" (r4-track-slug track))
                            (r4-one-line (r4-track-title track))))))
          (reverse r4-download--jobs)))

(defun r4-downloads--at-point ()
  "Return the download at point, or signal an error."
  (or (tabulated-list-get-id) (user-error "No download at point")))

(defun r4-downloads-cancel ()
  "Cancel the download at point."
  (interactive)
  (r4-download-cancel (r4-downloads--at-point))
  (revert-buffer))

(defun r4-downloads-retry ()
  "Queue again the download at point."
  (interactive)
  (r4-download-retry (r4-downloads--at-point))
  (revert-buffer))

(defun r4-downloads-retry-failed ()
  "Queue again all the failed downloads of the list."
  (interactive)
  (let ((failed (cl-remove-if-not (lambda (job) (eq (r4-download-status job) 'failed))
                                  r4-download--jobs)))
    (mapc #'r4-download-retry failed)
    (message "Retrying %d download%s" (length failed) (if (cdr failed) "s" ""))
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

(defun r4-downloads-visit-file ()
  "Visit the downloaded file at point in Dired."
  (interactive)
  (let* ((job (r4-downloads--at-point))
         (file (or (r4-files-track-file (r4-download-track job) (r4-download-kind job))
                   (user-error "Not downloaded"))))
    (dired-jump nil file)))

(defvar-keymap r4-downloads-mode-map
  :doc "Keymap for `r4-downloads-mode'."
  "x" #'r4-downloads-cancel
  "X" #'r4-download-cancel-all
  "r" #'r4-downloads-retry
  "R" #'r4-downloads-retry-failed
  "c" #'r4-downloads-clear
  "l" #'r4-downloads-show-log
  "F" #'r4-downloads-visit-file
  "o" #'r4-files-open-directory)

(define-derived-mode r4-downloads-mode tabulated-list-mode "r4 downloads"
  "Major mode listing Radio4000 downloads.

\\{r4-downloads-mode-map}"
  (setq tabulated-list-format [("Status" 10 t)
                               ("Progress" 9 t :right-align t)
                               ("Kind" 6 t)
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

(provide 'r4-downloads)

;;; r4-downloads.el ends here
