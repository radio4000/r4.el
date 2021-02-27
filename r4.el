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



;; r4 player
(defun r4-kill-player ()
  (interactive)
  (let ((proc (get-process "r4-player")))
    (when proc (kill-process proc))))

(defun msg-me (process event)
  (message (format "Process: %s had the event '%s'" process event)))

(define-derived-mode r4-player-mode tabulated-list-mode "r4-plyer"
  "Major mode for handling r4."
  (kill-all-local-variables)
  (setq major-mode 'r4-mode)
  (setq mode-name "R4 Player")
  (setq tabulated-list-format [("id" 10 t)
			       ("title" 30 t)])
  (setq tabulated-list-sort-key (cons "id" nil))
  (tabulated-list-init-header)
  (tabulated-list-print))

(define-button-type 'r4-play-track
  'action 'button-r4-player-play-track-action)

(defun button-r4-player-play-track-action (button)
  "Defines an action for a 'button' used in
tabulated-list-mode. It plays a track"
  (let ((track-data (if button (button-get button 'action-data)
			(tabulated-list-get-id))))
    (if track-data
        (r4-player-play-track track-data)
      (user-error "No track here"))))

(defun r4-player-play-track (track-data)
  (r4-kill-player)
  (message (format "Playing: %s" (cdr (assoc 'title track-data))))
  (start-process "r4-player" "**r4-player**" "mpv" (cdr (assoc 'url track-data))))

(defun r4-player-play-tracks (tracks-data)
  "Create a new r4-player buffer with a list of tracks to be played"
  (let* ((r4-buffer-name "**r4-player-status**")
	 (r4-buffer (generate-new-buffer r4-buffer-name)))
    (with-current-buffer r4-buffer
      ;; Since some channels include non-ASCII characters
      (setq buffer-file-coding-system 'utf-8)
      (setq tabulated-list-entries nil)
      (dolist (track tracks-data)
	(push (list (format "%s" (cdr (assoc 'id track))) (vector
							   (format "%s" (cdr (assoc 'id track)))
							   (list (cdr (assoc 'title track)) :type 'r4-play-track 'action-data track)))
	      tabulated-list-entries))
      (switch-to-buffer r4-buffer)
      (r4-player-mode))))

;; r4 explorer
(defun sort-tracks-predicate (tabulated-item-a tabulated-item-b)
  (>
   (string-to-number (elt (car (cdr tabulated-item-a)) 2))
   (string-to-number (elt (car (cdr tabulated-item-b)) 2))))

(define-derived-mode r4-mode tabulated-list-mode "r4"
  "Major mode for handling r4."
  (kill-all-local-variables)
  (setq major-mode 'r4-mode)
  (setq mode-name "R4")
  (setq tabulated-list-format [("slug" 25 t)
			       ("title" 30 t)
			       ("tracks" 10 sort-tracks-predicate)
			       ("body" 50 t)])
  (setq tabulated-list-sort-key (cons "slug" (cons "title" (cons "tracks" nil))))
  (set-keymap-parent (make-sparse-keymap) tabulated-list-mode-map)
  (tabulated-list-init-header)
  (tabulated-list-print))

;; enpoints urls
(defvar-local r4-api-channels-url "https://radio4000.firebaseio.com/channels.json"
  "The url pointing to the r4 api channels listing.")

(defun get-track-api-url (track-id)
  (format "https://radio4000.firebaseio.com/tracks/%s.json" track-id))

(defun get-channel-tracks-api-url (channel-id)
  (format "https://radio4000.firebaseio.com/tracks.json?orderBy=\"channel\"&startAt=\"%s\"&endAt=\"%s\"" channel-id channel-id))

;; api.r4 json endpoints serializers
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

(defun serialize-channel-tracks (res)
  "Serializes a request json response containing all r4 channels"
  (let* ((serialized-tracks)
	 (tracks (request-response-data res)))
    (dolist (track tracks)
      (let* ((track-id (car track))
	     (track-data (cdr track))
	     (track-tracks-length (length (cdr (assoc 'tracks track-data))))
	     (new-track (copy-alist track-data)))
	(push (cons 'id track-id) new-track)
	(push new-track serialized-tracks)))
    serialized-tracks))

(defun serialize-track (track-id track-response)
  "Serializes a request json response containing all r4 channels"
  (let* ((serialized-track (copy-alist (cdr track-response))))
    (push (cons 'id track-id) serialized-track)
    serialized-track))

;; api.r4 get json endpoints/models methods
(defun r4-api-get-channels ()
  "Make a request to the api.r4 endpoint, to get all
channels.json"
  (request r4-api-channels-url
    :sync t
    :type "GET"
    :parser 'json-read
    :error (cl-function
	    (lambda (&key response &allow-other-keys)))))

(defun r4-api-get-channel-tracks (channel-tracks-url)
  "Make a request to the api.r4 endpoint, to get all
channels.json"
  (request channel-tracks-url
    :sync t
    :type "GET"
    :parser 'json-read
    :error (cl-function
	    (lambda (&key response &allow-other-keys)))))

(defun r4-api-get-track (track-id)
  "Make a request to the api.r4 endpoint, to get all
channels.json"
  (let ((request-backend 'url-retrieve))
    (request (get-track-api-url track-id)
      :sync t
      :type "GET"
      :parser 'json-read
      :error (cl-function
	      (lambda (&key response &allow-other-keys))))))

(defun r4-api-get-track-async (track-id got-track-fn)
  "Make a request to the api.r4 endpoint, to get all
channels.json"
  (let ((request-backend 'url-retrieve))
    (request (get-track-api-url track-id)
      :type "GET"
      :data '((cons 'id track-id))
      :parser 'json-read
      :success (cl-function
		(lambda (&response response &allow-other-keys)
		  (debug)
		  (funcall 'got-track-fn response track-id)))
      :error (cl-function
	      (lambda (&key response &allow-other-keys))))))

;; r4 mode interactive methods
;;;###autoload
(defalias 'list-r4-channels 'r4-status)

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

;; r4 mode, buttons and their actions
;; defines a new button type, to be used by the r4 tabulutaed list
;; mode; when selecting a channel
(define-button-type 'r4-xref
  'action 'button-r4-channel-action)

(defun button-r4-channel-action (button)
  "Defines an action for a 'button' used in
tabulated-list-mode. It opens a new Help buffer, with the
'clicked' channel data in it."
  (let ((channel-data (if button (button-get button 'action-data)
			(tabulated-list-get-id))))
    (if channel-data
        (r4-describe-channel channel-data)
      (user-error "No channel here"))))

;; could be an interactive function, if we suggest channels?
(defun r4-describe-channel (channel)
  "Display the full information about a r4 channel."
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (insert (format ".title: %s\n"
		      (propertize (cdr (assoc 'title channel))
				  'font-lock-face
				  'package-status-built-in)))
      (insert (format ".slug: @%s\n"
		      (cdr (assoc 'slug channel))))
      (insert (format ".body: %s\n" (cdr (assoc 'body channel))))
      (insert (format ".created: %s\n" (cdr (assoc 'created channel))))
      (insert (format ".updated: %s\n" (cdr (assoc 'updated channel))))
      (insert (format ".isFeatured: %s\n" (cdr (assoc 'isFeatured channel))))
      (insert (format ".link: %s\n" (cdr (assoc 'link channel))))
      (insert (format ".id: %s\n" (cdr (assoc 'id channel))))
      (insert (format ".channelPublic: %s\n" (cdr (assoc 'channelPublic channel))))
      (insert (format ".image: %s\n" (cdr (assoc 'image channel))))
      (insert (format ".coordinatesLatitude: %s\n" (cdr (assoc 'coordinatesLatitude channel))))
      (insert (format ".coordinatesLongitude: %s\n" (cdr (assoc 'coordinatesLongitude channel))))
      (insert (format ".tracksLength: %s\n" (cdr (assoc 'tracksLength channel))))
      (insert (format ".tracks:\n"))

      (let* ((channel-tracks-url (get-channel-tracks-api-url (cdr (assoc 'id channel))))
	     (tracks-res (r4-api-get-channel-tracks channel-tracks-url))
	     (tracks (serialize-channel-tracks tracks-res)))

	(insert " " (make-text-button "Play channel" nil 'action-data tracks 'action
                                      (lambda (button)
					(let ((tracks-data (if button (button-get button 'action-data)
							      (tabulated-list-get-id))))
					  (if tracks-data
					      (r4-player-play-tracks tracks-data)
					    (user-error "No channel.tracks here"))))) "/n")
	(dolist (track tracks)
	  (insert (format "%s" (cdr (assoc 'title track))))
	  (insert " " (make-text-button "play" nil 'action-data track 'action
                                    (lambda (button)
				      (let ((track-data (if button (button-get button 'action-data)
							    (tabulated-list-get-id))))
					(if track-data
					    (r4-player-play-track track-data)
					  (user-error "No track here"))))))
	  (insert (format "\n")))))))

;; (defun handle-track-res (response track-id)
;;   (let* ((track-res (request-response-data response))
;; 	 (track (serialize-track track-id track-res)))))

;; (dolist (track-pair (cdr (assoc 'tracks channel)))
;;   (let ((track-id (car track-pair)))
;;     (r4-api-get-track-async track-id 'handle-track-res)))


;; experimentations with how to play medias
;; (shell-command "mpv https://youtu.be/JbCnoym-pC0")
;; (start-process "r4-player" "**r4-player**" "mpv" "https://youtu.be/JbCnoym-pC0")


;; authenticated request
;; (request "http://httpbin.org/get"
;;   :auth "digest" ;; or "basic", "anyauth", etc., which see curl(1)
;;   :complete (cl-function
;;              (lambda (&key response &allow-other-keys)
;;                (message "Done: %s" (request-response-status-code response)))))

;;; r4.el ends here
