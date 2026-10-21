;;; r4-home.el --- The Radio4000 home buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `r4' opens the home buffer, like the home page of radio4000.com: the
;; player, your channels, the synced and local channels, the live
;; broadcasts, what you played lately, and links to the rest.  Every
;; item is a button, and ? opens the menu of all the commands.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'r4-models)
(require 'r4-api)
(require 'r4-auth)
(require 'r4-player)
(require 'r4-player-window)
(require 'r4-history)
(require 'r4-local)
(require 'r4-synced)
(require 'r4-broadcast)
(require 'r4-edit)
(require 'r4-channel-page)

(defcustom r4-home-history-size 5
  "Number of recently played tracks shown in the home buffer."
  :type 'natnum
  :group 'r4)

(defconst r4-home-buffer "*r4*"
  "Name of the home buffer.")

(defvar r4-home--my-channels nil
  "Your channels, once fetched, or `failed'.")

(defvar r4-home--broadcasts nil
  "The rows of the live broadcasts, once fetched, or `loading'.")

(declare-function r4-menu "r4-menus")
(declare-function r4-explore "r4-explore")
(declare-function r4-search "r4-search")
(declare-function r4-feed "r4-follows")
(declare-function r4-downloads "r4-downloads")

;;;; Content

(defun r4-home--heading (text)
  "Insert the heading TEXT."
  (insert "\n" (propertize text 'face 'r4-heading) "\n"))

(defun r4-home--button (label action &optional help)
  "Insert a button LABEL calling the function ACTION, with HELP."
  (insert-text-button label
                      'action (lambda (_) (funcall action))
                      'follow-link t
                      'help-echo help))

(defun r4-home--links (links)
  "Insert LINKS, a list of (LABEL . COMMAND), on one line.
Their key in the home buffer shows before them, like in magit."
  (pcase-dolist (`(,label . ,command) links)
    (if-let* ((key (where-is-internal command (current-local-map) t)))
        (r4-faces-insert-key (key-description key) label command)
      (r4-home--button label (lambda () (call-interactively command))
                       (documentation command t)))
    (insert "  "))
  (insert "\n"))

(defun r4-home--channels (slugs action)
  "Insert the channel SLUGS as buttons calling ACTION with a slug."
  (if (null slugs)
      (insert (propertize "None" 'face 'shadow))
    (dolist (slug slugs)
      (r4-home--button (concat "@" slug) (lambda () (funcall action slug)))
      (insert "  ")))
  (insert "\n"))

(defun r4-home--insert-player ()
  "Insert the player."
  (r4-player-window--insert
   (- (if-let* ((window (get-buffer-window (current-buffer) t)))
          (window-max-chars-per-line window)
        (frame-width))
      2)
   t)
  (insert "\n"))

(defun r4-home--insert-my-channels ()
  "Insert your channels, when signed in."
  (r4-home--heading "Your channels")
  (cond ((not (r4-auth-signed-in-p))
         (r4-home--links '(("Sign in" . r4-login)
                           ("Create local channel" . r4-local-create-channel))))
        ((eq r4-home--my-channels 'failed)
         (insert (propertize "Could not load them, g tries again" 'face 'shadow) "\n"))
        ((null r4-home--my-channels)
         (insert (propertize "Loading…" 'face 'shadow) "\n"))
        (t (r4-home--channels (mapcar #'r4-channel-slug r4-home--my-channels)
                              #'r4-channel-page)
           (r4-home--links '(("Feed" . r4-feed)
                             ("Create channel" . r4-create-channel))))))

(defun r4-home--insert-broadcasts ()
  "Insert the live broadcasts."
  (r4-home--heading "Live broadcasts")
  (pcase r4-home--broadcasts
    ('loading (insert (propertize "Loading…" 'face 'shadow) "\n"))
    ('nil (insert (propertize "None" 'face 'shadow) "\n"))
    (rows
     (dolist (row rows)
       (let-alist row
         (let ((slug .channels.slug)
               (channel-id .channel_id))
           (r4-home--button (concat "@" slug)
                            (lambda () (r4-broadcast-listen channel-id slug))
                            "Listen")
           (insert "  " (propertize (r4-one-line (alist-get 'track_title (r4-broadcast-deck row)))
                                    'face 'shadow)
                   "\n")))))))

(defun r4-home--insert-history ()
  "Insert the tracks played lately."
  (r4-history--load)
  (r4-home--heading "Played lately")
  (if (null r4-history--entries)
      (insert (propertize "Nothing yet" 'face 'shadow) "\n")
    (cl-loop for (_time . track) in r4-history--entries
             for count from 0 below r4-home-history-size
             do (let ((track track))
                  (insert "  ")
                  (r4-home--button (r4-one-line (r4-track-title track))
                                   (lambda () (r4-player-play (list track)))
                                   "Play")
                  (insert "  " (propertize (concat "@" (r4-track-slug track)) 'face 'shadow)
                          "\n")))
    (r4-home--links '(("History" . r4-history)))))

(defun r4-home--insert ()
  "Insert the home buffer."
  (insert (propertize "Radio4000" 'face 'r4-big-heading) "\n\n")
  (r4-home--insert-player)
  (insert "\n")
  (r4-home--links '(("Channels" . r4-channels)
                    ("Visit channel" . r4-channel-page)
                    ("Explore" . r4-explore)
                    ("Search" . r4-search)
                    ("Broadcasts" . r4-broadcasts)
                    ("History" . r4-history)
                    ("Downloads" . r4-downloads)
                    ("All commands" . r4-menu)))
  (r4-home--insert-my-channels)
  (r4-home--heading "Synced channels")
  (r4-home--channels r4-synced-channels #'r4-channel-page)
  (r4-home--heading "Local channels")
  (r4-home--channels (r4-local-slugs) #'r4-local-open-channel)
  (r4-home--insert-broadcasts)
  (r4-home--insert-history))

(defun r4-home--render ()
  "Redraw the home buffer, if any, keeping the position."
  (when-let* ((buffer (get-buffer r4-home-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (position (point)))
        (erase-buffer)
        (r4-home--insert)
        (goto-char (min position (point-max)))))))

(defun r4-home--refresh-player ()
  "Redraw the home buffer when the player changed, if it is shown."
  (when (get-buffer-window r4-home-buffer t)
    (r4-home--render)))

(add-hook 'r4-player-changed-hook #'r4-home--refresh-player)

;;;; Fetching

(defun r4-home--fetch-my-channels ()
  "Fetch your channels, then show them."
  (when (r4-auth-signed-in-p)
    (setq r4-home--my-channels nil)
    (run-at-time 0 nil
                 (lambda ()
                   (setq r4-home--my-channels
                         (condition-case nil
                             (or (r4-edit-my-channels) 'failed)
                           (error 'failed)))
                   (r4-home--render)))))

(defun r4-home--fetch-broadcasts ()
  "Fetch the live broadcasts, then show them."
  (setq r4-home--broadcasts 'loading)
  (let ((r4-api-error-function (lambda (&rest _)
                                 (setq r4-home--broadcasts nil)
                                 (r4-home--render))))
    (r4-api-get "broadcast" `(("select" ,r4-broadcast--columns) ("order" "updated_at.desc"))
                (lambda (rows)
                  (setq r4-home--broadcasts rows)
                  (r4-home--render)))))

(defun r4-home-revert (&rest _)
  "Fetch again what the home buffer shows."
  (r4-home--fetch-my-channels)
  (r4-home--fetch-broadcasts)
  (r4-home--render))

(defvar-keymap r4-home-mode-map
  :doc "Keymap for `r4-home-mode'."
  :parent (make-composed-keymap r4-player-map special-mode-map)
  "TAB" #'forward-button
  "<backtab>" #'backward-button
  "?" #'r4-menu
  ;; The player's own views: share the playing track.
  "w" #'r4-player-copy-url
  "W" #'r4-player-copy-web-url
  "o" #'r4-player-browse-current
  "O" #'r4-player-browse-media)

(define-derived-mode r4-home-mode special-mode "r4"
  "Major mode of the Radio4000 home buffer.

\\{r4-home-mode-map}"
  (setq-local revert-buffer-function #'r4-home-revert))

(defvar-local r4-up-function nil
  "Function showing the view above the current buffer, or nil for home.")

(defun r4-up ()
  "Go up, like in Dired: from the tracks of a channel to its page, up to home."
  (interactive)
  (if (derived-mode-p 'r4-home-mode)
      (message "r4: this is the top, home")
    (funcall (or r4-up-function #'r4))))

;;;###autoload
(defun r4 ()
  "Show the Radio4000 home buffer, like the home page of radio4000.com.
? in it opens the menu of all the commands, `r4-menu'."
  (interactive)
  (with-current-buffer (get-buffer-create r4-home-buffer)
    (unless (derived-mode-p 'r4-home-mode)
      (r4-home-mode))
    (r4-home-revert)
    (goto-char (point-min))
    (pop-to-buffer-same-window (current-buffer))))

(provide 'r4-home)

;;; r4-home.el ends here
