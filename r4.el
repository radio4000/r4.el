;;; r4.el --- Browse radio4000.com

;; Copyright (C) 2021 Free Software Foundation, Inc.


;; Author: H. Ugrp  <h.u.g.u.r.p+emacs@pm.me>
;; Version: 0.1
;; Package-Requires: (request tabulated-list)
;; Keywords: radio4000
;; URL: https://gitlab.com/hugurp/r4.el

;;; Commentary:

;; This package provides a major mode for browsing the data on
;; radio400.com

(require 'tabulated-list)
(require 'request nil 'noerror)

(define-derived-mode r4-mode tabulated-list-mode "r4"
  "Major mode for handling r4."
  (kill-all-local-variables)
  (setq major-mode 'r4-mode)
  (setq mode-name "R4")
  (setq tabulated-list-format [("slug" 25 t)
			       ("title" 30 t)
			       ("tracks" 10 t)
			       ("body" 50 t)])
  (setq tabulated-list-sort-key (cons "slug" (cons "title" (cons "tracks" nil))))
  (set-keymap-parent (make-sparse-keymap) tabulated-list-mode-map)
  (tabulated-list-init-header)
  (tabulated-list-print))


(defvar-local r4-api-channels-url "https://radio4000.firebaseio.com/channels.json"
  "The url pointing to the r4 api channels listing.")

(defun serialize-channels (res)
  "Serializes a request json response containing all r4 channels"
  (let* ((serialized-channels)
	 (channels res))
    (dolist (channel channels)
      (let* ((channel-id (car channel))
	     (channel-data (cdr channel))
	     (channel-tracks-length (length (cdr (assoc 'tracks channel-data))))
	     (new-channel (copy-alist channel-data)))
	(push (cons 'id channel-id) new-channel)
	(push (cons 'tracksLength channel-tracks-length) new-channel)
	(push new-channel serialized-channels)))
    serialized-channels))

(defun r4-api-get-channels ()
  "Make a request to the api.r4 endpoint, to get all
channels.json"
  (request r4-api-channels-url
    :sync t
    :type "GET"
    :parser 'json-read
    :error (cl-function
	    (lambda (&key response &allow-other-keys)))))

(defun r4-status ()
  "Create a new r4 buffer with the status of the site. It will
show the channels"
  (interactive)
  (let* ((channels-res (request-response-data (r4-api-get-channels)))
	 (channels (serialize-channels channels-res))
	 (r4-buffer-name "**r4-status**")
	 (r4-buffer (generate-new-buffer r4-buffer-name)))
    (with-current-buffer r4-buffer
      ;; Since some channels include non-ASCII characters
      (setq buffer-file-coding-system 'utf-8)
      (setq tabulated-list-entries nil)
      (dolist (channel channels)
	(push (list (cdr (assoc 'id channel)) (vector
					       (list (cdr (assoc 'slug channel)) :type 'r4-xref 'action-data channel)
					       (cdr (assoc 'title channel))
					       (format "%s" (cdr (assoc 'tracksLength channel)))
					       (format "%s" (or (cdr (assoc 'body channel)) ""))))
	      tabulated-list-entries))
      (switch-to-buffer r4-buffer)
      (r4-mode))))

;; defines a new button type, to be used by the r4 tabulutaed list
;; mode; when selecting a channel
(define-button-type 'r4-xref
  'action 'button-r4-channel-action)

(defun button-r4-channel-action (button)
  "Defines an action for a 'button' used in
tabulated-list-mode. It opens a new Help buffer, with the
'clicked' channel data in it."
  (let* ((channel-data (button-get button 'action-data))
	 (r4-channel-buffer-name "**r4-channel**")
	 (r4-channel-buffer (generate-new-buffer r4-channel-buffer-name)))
    (with-current-buffer r4-channel-buffer
      (message "%s" channel-data)
      (switch-to-buffer r4-channel-buffer))))

(defun r4-describe-channel()
  "Shows a r4 channel's informations in a new buffer."
  (message "DESSSSSCCC"))

;;;###autoload
(defalias 'list-r4-channels 'r4-status)

;;; r4.el ends here
