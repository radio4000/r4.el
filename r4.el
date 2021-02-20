;;; r4.el --- Browse radio4000.com

;; Copyright (C) 2021 Free Software Foundation, Inc.


;; Author: H. Ugrp  <h.u.g.u.r.p+emacs@pm.me>
;; Version: 0.1
;; Package-Requires: ((request "0.3.3"))
;; Keywords: radio4000
;; URL: https://gitlab.com/hugurp/r4.el

;;; Commentary:

;; This package provides a major mode for browsing the data on
;; radio400.com

(require 'request nil 'noerror)

(define-derived-mode r4-mode tabulated-list-mode "r4"
  "Major mode for handling r4."
  (kill-all-local-variables)
  (setq major-mode 'r4-mode)
  (setq mode-name "R4")
  (setq tabulated-list-format [("slug" 30 t)
			       ("title" 50 t)
			       ("body" 100 t)])
  (tabulated-list-init-header)
  (tabulated-list-print))

(defvar-local r4-api-channels-url "https://radio4000.firebaseio.com/channels.json"
  "The url pointing to the r4 api channels listing.")

(defun r4-api-get-channels ()
  (request r4-api-channels-url
    :sync t
    :type "GET"
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(message "r4 fetched all channels")
		data))
    :error (cl-function
	    (lambda (&key response &allow-other-keys)))))

(defun r4-status ()
  "Create a new r4 buffer with the status of the site."
  (interactive)
  (let* ((channels-res (r4-api-get-channels))
	 (r4-buffer-name "**r4-status**")
	 (r4-buffer (generate-new-buffer r4-buffer-name)))
    (with-current-buffer r4-buffer
      (setq tabulated-list-entries nil)
      (dolist (channel (request-response-data channels-res))
	(let* ((channel-id (car channel))
	       (channel-data (cdr channel)))
	  (push (list channel-id (vector
				  (cdr (assoc 'slug channel-data))
				  (cdr (assoc 'title channel-data))
				  (or (cdr (assoc 'body channel-data)) "")))
		tabulated-list-entries)))
      (switch-to-buffer r4-buffer)
      (r4-mode))))

;;;###autoload
(defalias 'list-r4-channels 'r4-status)
