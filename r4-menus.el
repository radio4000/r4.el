;;; r4-menus.el --- Radio4000 menus  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Transient menus: `r4', with all the commands, and the menus of the
;; lists, on ? in each list.

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

(eval-and-compile
  (defconst r4-menus--player
    ["Player"
     ("SPC" "Pause/resume" r4-player-toggle-pause :transient t)
     (">" "Next" r4-player-next :transient t)
     ("<" "Previous" r4-player-previous :transient t)
     ("]" "Seek forward" r4-player-seek-forward :transient t)
     ("[" "Seek backward" r4-player-seek-backward :transient t)
     ("+" "Volume up" r4-player-volume-up :transient t)
     ("-" "Volume down" r4-player-volume-down :transient t)
     ("Z" "Toggle shuffle" r4-player-toggle-shuffle :transient t)
     ("V" "Toggle video" r4-player-toggle-video :transient t)
     ("Q" "Queue" r4-queue)
     ("k" "Stop" r4-player-stop)]
    "The player group of the r4 menus."))

(defmacro r4-menus--define (name doc &rest groups)
  "Define the transient menu NAME with DOC, GROUPS and the player keys."
  (declare (indent 1) (doc-string 2))
  `(transient-define-prefix ,name ()
     ,doc
     ,@groups
     ,r4-menus--player))

;;;###autoload (autoload 'r4 "r4-menus" nil t)
(r4-menus--define r4
  "Radio4000 menu."
  [["Browse"
    ("c" "Channels" r4-channels)
    ("C" "Search channels" r4-search-channels)
    ("s" "Search tracks: @channel #tag words" r4-search)
    ("@" "Tracks mentioning a channel" r4-mentions)
    ("o" "Open channel" r4-open-channel)
    ("i" "Describe channel" r4-describe-channel)
    ("h" "History" r4-history)]
   ["Current track"
    ("P" "Play channel" r4-play-channel)
    ("R" "Auto-radio of channel" r4-auto-radio)
    ("w" "Copy media URL" r4-player-copy-url)
    ("W" "Copy radio4000.com URL" r4-player-copy-web-url)
    ("e" "Open on radio4000.com" r4-player-browse-current)]]
  [["Download"
    ("D" "Download channel" r4-download-channel)
    ("L" "Downloads" r4-downloads)
    ("F" "Open music folder" r4-files-open-directory)
    ("K" "Cancel all downloads" r4-download-cancel-all)]
   ["Local"
    ("l" "Open local channel" r4-local-open-channel)
    ("N" "New local channel" r4-local-create-channel)
    ("a" "Add track by URL" r4-local-add-track)
    ("S" "Save channel locally" r4-local-save-channel)]
   ["Account"
    ("y" "My channels" r4-my-channels)
    ("+" "Add a track to my channel" r4-add-track)
    ("f" "Feed of followed channels" r4-feed)
    ("m" "Channels I follow" r4-following)
    ("M" "My followers" r4-followers)
    ("Y" "Sync local channel" r4-sync-channel)
    ("G" "Get one of my channels" r4-sync-pull-channel)
    ("I" "Sign in" r4-login)
    ("O" "Sign out" r4-logout)
    ("U" "Who am I" r4-whoami)]])

;;;###autoload (autoload 'r4-tracks-menu "r4-menus" nil t)
(r4-menus--define r4-tracks-menu
  "Menu of a list of tracks."
  [["Play"
    ("RET" "From this track" r4-tracks-play)
    ("P" "All" r4-tracks-play-all)
    ("z" "Shuffle" r4-tracks-shuffle)
    ("R" "Auto-radio" r4-tracks-auto-radio)
    ("a" "Add to queue" r4-tracks-enqueue)
    ("N" "Play next" r4-tracks-play-next)]
   ["Select"
    ("m" "Mark" r4-tracks-mark :transient t)
    ("u" "Unmark" r4-tracks-unmark :transient t)
    ("M" "Mark all" r4-tracks-mark-all :transient t)
    ("U" "Unmark all" r4-tracks-unmark-all :transient t)]
   ["Download"
    ("d" "Marked or at point" r4-tracks-download)
    ("D" "All" r4-tracks-download-all)
    ("F" "Visit file" r4-tracks-open-file)
    ("L" "Downloads" r4-downloads)]]
  [["Find"
    ("s" "Search: #tag words" r4-tracks-search)
    ("/" "Filter shown tracks" r4-tracks-filter)
    ("#" "Filter by tags" r4-tracks-filter-tag)
    ("@" "Open channel of track" r4-tracks-open-channel)
    ("g" "Refresh" revert-buffer)
    ("S" "Sort by column" tabulated-list-sort)]
   ["Edit (your or local channel)"
    ("+" "Add a track" r4-tracks-create)
    ("e" "Edit track" r4-tracks-edit)
    ("x" "Delete tracks" r4-tracks-delete)]
   ["Local channels"
    ("c" "Copy to a local channel" r4-tracks-copy-to-local)
    ("C" "Save this channel locally" r4-tracks-save-local)
    ("Y" "Sync local channel" r4-tracks-sync)]
   ["Links"
    ("o" "Open on radio4000.com" r4-tracks-browse)
    ("O" "Open media" r4-tracks-browse-media)
    ("w" "Copy media URL" r4-tracks-copy-url)
    ("W" "Copy radio4000.com URL" r4-tracks-copy-web-url)]])

;;;###autoload (autoload 'r4-channels-menu "r4-menus" nil t)
(r4-menus--define r4-channels-menu
  "Menu of a list of channels."
  [["Channel"
    ("RET" "Open" r4-channels-open)
    ("P" "Play" r4-channels-play)
    ("z" "Shuffle" r4-channels-shuffle)
    ("R" "Auto-radio" r4-channels-auto-radio)
    ("i" "Describe" r4-channels-describe)
    ("D" "Download" r4-channels-download)
    ("C" "Save locally" r4-channels-save-local)]
   ["Edit (yours)"
    ("+" "Create a channel" r4-create-channel)
    ("e" "Edit" r4-channels-edit)
    ("x" "Delete" r4-channels-delete)]
   ["Find"
    ("s" "Search channels" r4-search-channels)
    ("t" "Search tracks" r4-search)
    ("M" "Tracks mentioning it" r4-channels-mentions)]
   ["Follow"
    ("f" "Follow or unfollow" r4-channels-follow)
    ("F" "Channels it follows" r4-channels-following)
    ("r" "Its followers" r4-channels-followers)
    ("g" "Refresh" revert-buffer)
    ("S" "Sort by column" tabulated-list-sort)]
   ["Links"
    ("o" "Open on radio4000.com" r4-channels-browse)
    ("w" "Copy radio4000.com URL" r4-channels-copy-url)]])

;;;###autoload (autoload 'r4-queue-menu "r4-menus" nil t)
(r4-menus--define r4-queue-menu
  "Menu of the play queue."
  [["Queue"
    ("RET" "Play" r4-queue-play)
    ("x" "Remove" r4-queue-remove)
    ("C" "Clear" r4-player-clear)]
   ["Links"
    ("o" "Open on radio4000.com" r4-queue-browse)
    ("w" "Copy media URL" r4-queue-copy-url)
    ("W" "Copy radio4000.com URL" r4-queue-copy-web-url)]])

(keymap-set r4-tracks-mode-map "?" #'r4-tracks-menu)
(keymap-set r4-channels-mode-map "?" #'r4-channels-menu)
(keymap-set r4-queue-mode-map "?" #'r4-queue-menu)

(provide 'r4-menus)

;;; r4-menus.el ends here
