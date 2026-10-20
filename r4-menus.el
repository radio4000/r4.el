;;; r4-menus.el --- Radio4000 menus  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Transient menus: `r4', with all the commands, and the menus of the
;; lists, on ? in each list.  Each menu ends with the player, headed by
;; the playing track, with the same keys as in every list.

;;; Code:

(require 'transient)
(require 'r4-player)
(require 'r4-queue)
(require 'r4-files)
(require 'r4-downloads)
(require 'r4-tracks)
(require 'r4-channels)
(require 'r4-history)
(require 'r4-search)
(require 'r4-local)
(require 'r4-auth)
(require 'r4-sync)
(require 'r4-follows)
(require 'r4-edit)
(require 'r4-auto-radio)
(require 'r4-broadcast)
(require 'r4-describe)
(require 'r4-explore)

;;;; Descriptions showing the state

(defun r4-menus--on-off (value)
  "Return VALUE as on or off, highlighted when on."
  (if value (propertize "on" 'face 'transient-value) (propertize "off" 'face 'shadow)))

(defun r4-menus--shuffle ()
  "Return the description of the shuffle toggle."
  (concat "Shuffle " (r4-menus--on-off r4-player-shuffle)))

(defun r4-menus--video ()
  "Return the description of the video toggle."
  (concat "Video " (r4-menus--on-off r4-player-video)))

(defun r4-menus--player ()
  "Return the heading of the player group: the playing track."
  (concat (propertize "Player" 'face 'transient-heading) "  "
          (if-let* ((track (and (r4-player-live-p) (r4-player-current))))
              (format "%s %s  %s" (if r4-player-paused "⏸" "▶")
                      (r4-one-line (r4-track-title track))
                      (propertize (or r4-player-label (concat "@" (r4-track-slug track)))
                                  'face 'shadow))
            (propertize "nothing playing" 'face 'shadow))))

(defun r4-menus--playing-p ()
  "Return non-nil if a track is playing."
  (and (r4-player-live-p) (r4-player-current)))

(defun r4-menus--local-list-p ()
  "Return non-nil in the list of a local channel."
  (plist-get r4-tracks--source :local))

(defun r4-menus--remote-list-p ()
  "Return non-nil in a list of tracks of radio4000.com."
  (not (r4-menus--local-list-p)))

(defun r4-menus--playing-remote-p ()
  "Return non-nil if the playing track is on radio4000.com."
  (and (r4-menus--playing-p) (not (r4-track-local-p (r4-player-current)))))

(defun r4-menus--filter ()
  "Return the description of the filter of the track list."
  (if r4-search--filter-text
      (format "Filter: %s" (propertize r4-search--filter-text 'face 'transient-value))
    "Filter shown tracks"))

(defun r4-menus--account ()
  "Return the heading of the account menu."
  (if-let* ((session (ignore-errors (r4-auth-session))))
      (format "Signed in as %s" (plist-get session :email))
    "Not signed in"))

;;;; Shared groups

(eval-and-compile
  (defconst r4-menus--player-group
    [:description r4-menus--player
     [("SPC" "Pause/resume" r4-player-toggle-pause :transient t)
      (">" "Next track" r4-player-next :transient t)
      ("<" "Previous track" r4-player-previous :transient t)
      ("k" "Stop" r4-player-stop)]
     [("]" "Seek forward" r4-player-seek-forward :transient t)
      ("[" "Seek backward" r4-player-seek-backward :transient t)
      ("+" "Volume up" r4-player-volume-up :transient t)
      ("-" "Volume down" r4-player-volume-down :transient t)]
     [("Z" r4-player-toggle-shuffle :description r4-menus--shuffle :transient t)
      ("V" r4-player-toggle-video :description r4-menus--video :transient t)
      ("Q" "Queue" r4-queue)]]
    "The player group of the r4 menus."))

(defmacro r4-menus--define (name doc &rest groups)
  "Define the transient menu NAME with DOC, GROUPS and the player."
  (declare (indent 1) (doc-string 2))
  `(transient-define-prefix ,name ()
     ,doc
     ,@groups
     ,r4-menus--player-group))

;;;; Menus

;;;###autoload (autoload 'r4 "r4-menus" nil t)
(r4-menus--define r4
  "Radio4000 menu."
  [["Channels"
    ("c" "Channels" r4-channels)
    ("x" "Explore" r4-explore)
    ("o" "Visit channel" r4-open-channel)
    ("i" "Describe channel" r4-describe-channel)
    ("@" "Mentions" r4-mentions)]
   ["Play"
    ("P" "Play channel" r4-play-channel)
    ("R" "Auto radio" r4-auto-radio)
    ("b" "Broadcasts" r4-broadcasts)]
   ["Tracks"
    ("s" "Search tracks: @slug #tag" r4-search)
    ("h" "Listening history" r4-history)]
   ["Download"
    ("D" "Download channel" r4-download-channel)
    ("L" "Downloads" r4-downloads)
    ("F" "Downloads folder" r4-files-open-directory)]
   ["Local"
    ("l" "Local channels…" r4-local-menu)]
   ["Account"
    ("a" "Account…" r4-account-menu)]]
  ["Share the playing track"
   :if r4-menus--playing-p
   ("w" "Copy media URL" r4-player-copy-url)
   ("W" "Copy radio4000.com URL" r4-player-copy-web-url :if r4-menus--playing-remote-p)
   ("e" "Open on radio4000.com" r4-player-browse-current :if r4-menus--playing-remote-p)])

;;;###autoload (autoload 'r4-local-menu "r4-menus" nil t)
(r4-menus--define r4-local-menu
  "Menu of the local channels."
  [["Local channels"
    ("l" "Visit local channel" r4-local-open-channel)
    ("N" "Create local channel" r4-local-create-channel)
    ("A" "Add track" r4-local-add-track)
    ("S" "Backup a channel" r4-local-save-channel)]
   ["Sync"
    ("Y" "Sync local channel" r4-sync-channel)
    ("G" "Get my channel" r4-sync-pull-channel)]
   ["Downloads"
    ("M" "Migrate r4.el 0.2 downloads" r4-files-migrate)]])

;;;###autoload (autoload 'r4-account-menu "r4-menus" nil t)
(r4-menus--define r4-account-menu
  "Menu of your account."
  [:description r4-menus--account
   ["Account"
    ("I" "Log in" r4-login)
    ("O" "Log out" r4-logout)
    ("y" "My channels" r4-my-channels)]
   ["Follows"
    ("f" "Feed" r4-feed)
    ("m" "Following" r4-following)
    ("M" "Followers" r4-followers)]
   ["Edit"
    ("A" "Add track" r4-add-track)
    ("C" "Create channel" r4-create-channel)
    ("E" "Edit channel" r4-edit-channel)
    ("X" "Delete channel" r4-delete-channel)]])

;;;###autoload (autoload 'r4-tracks-menu "r4-menus" nil t)
(r4-menus--define r4-tracks-menu
  "Menu of a list of tracks."
  [["Play"
    ("RET" "Play track" r4-tracks-play)
    ("P" "Play all" r4-tracks-play-all)
    ("z" "Shuffle" r4-tracks-shuffle)
    ("R" "Auto radio" r4-tracks-auto-radio)
    ("a" "Add to queue" r4-tracks-add-to-queue)
    ("N" "Play next" r4-tracks-play-next)]
   ["Search"
    ("s" "Search: #tag words" r4-tracks-search)
    ("/" r4-tracks-filter :description r4-menus--filter)
    ("#" "Filter by tags" r4-tracks-filter-tag)
    ("i" "Describe track" r4-tracks-describe)
    ("@" "Go to channel" r4-tracks-open-channel :if r4-menus--remote-list-p)
    ("S" "Sort by column" tabulated-list-sort)
    ("g" "Refresh" revert-buffer)]
   ["Select"
    ("m" "Mark" r4-tracks-mark :transient t)
    ("u" "Unmark" r4-tracks-unmark :transient t)
    ("M" "Mark all" r4-tracks-mark-all :transient t)
    ("U" "Unmark all" r4-tracks-unmark-all :transient t)]]
  [["Download"
    ("d" "Marked or at point" r4-tracks-download)
    ("D" "All" r4-tracks-download-all)
    ("F" "Visit file" r4-tracks-open-file)
    ("L" "Downloads" r4-downloads)]
   ["Edit"
    ("A" "Add track" r4-tracks-create)
    ("e" "Edit track" r4-tracks-edit)
    ("x" "Delete track" r4-tracks-delete)]
   ["Local"
    ("c" "Add to local channel" r4-tracks-copy-to-local)
    ("C" "Backup channel" r4-tracks-save-local :if r4-menus--remote-list-p)
    ("Y" "Sync local channel" r4-tracks-sync :if r4-menus--local-list-p)]
   ["Share"
    ("o" "Open on radio4000.com" r4-tracks-browse :if r4-menus--remote-list-p)
    ("O" "Open media" r4-tracks-browse-media)
    ("w" "Copy media URL" r4-tracks-copy-url)
    ("W" "Copy radio4000.com URL" r4-tracks-copy-web-url :if r4-menus--remote-list-p)]])

;;;###autoload (autoload 'r4-channels-menu "r4-menus" nil t)
(r4-menus--define r4-channels-menu
  "Menu of a list of channels."
  [["Channel"
    ("RET" "Visit channel" r4-channels-open)
    ("P" "Play channel" r4-channels-play)
    ("z" "Shuffle" r4-channels-shuffle)
    ("R" "Auto radio" r4-channels-auto-radio)
    ("i" "Describe channel" r4-channels-describe)]
   ["Search"
    ("s" "Search channels" r4-search-channels)
    ("/" "Filter shown channels" r4-channels-filter)
    ("t" "Search tracks" r4-search)
    ("M" "Mentions" r4-channels-mentions)
    ("S" "Sort by column" tabulated-list-sort)
    ("g" "Refresh" revert-buffer)]
   ["Follows"
    ("f" "Follow/unfollow" r4-channels-follow)
    ("F" "Following" r4-channels-following)
    ("r" "Followers" r4-channels-followers)]]
  [["Download"
    ("D" "Download channel" r4-channels-download)
    ("C" "Backup channel" r4-channels-save-local)]
   ["Edit"
    ("A" "Create channel" r4-create-channel)
    ("e" "Edit channel" r4-channels-edit)
    ("x" "Delete channel" r4-channels-delete)]
   ["Share"
    ("o" "Open on radio4000.com" r4-channels-browse)
    ("w" "Copy radio4000.com URL" r4-channels-copy-url)]])

;;;###autoload (autoload 'r4-queue-menu "r4-menus" nil t)
(r4-menus--define r4-queue-menu
  "Menu of the play queue."
  [["Queue"
    ("RET" "Play track" r4-queue-play)
    ("x" "Remove" r4-queue-remove)
    ("C" "Clear" r4-player-clear)]
   ["Share"
    ("o" "Open on radio4000.com" r4-queue-browse)
    ("w" "Copy media URL" r4-queue-copy-url)
    ("W" "Copy radio4000.com URL" r4-queue-copy-web-url)]])

(keymap-set r4-tracks-mode-map "?" #'r4-tracks-menu)
(keymap-set r4-channels-mode-map "?" #'r4-channels-menu)
(keymap-set r4-queue-mode-map "?" #'r4-queue-menu)

(provide 'r4-menus)

;;; r4-menus.el ends here
