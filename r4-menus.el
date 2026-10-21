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
(require 'r4-synced)
(require 'r4-channel-page)
(require 'r4-home)
(require 'r4-broadcasting)

;;;; Descriptions showing the state

;; Transient formats descriptions in a temporary buffer: those showing
;; the state of a list read it in the buffer the menu was opened from.
(defmacro r4-menus--in-buffer (&rest body)
  "Run BODY in the buffer the menu was opened from."
  (declare (indent 0))
  `(with-current-buffer (or (and (buffer-live-p transient--original-buffer)
                                 transient--original-buffer)
                            (current-buffer))
     ,@body))

(defun r4-menus--heading (title &rest parts)
  "Return a menu heading: TITLE, then the non-nil PARTS, dimmed."
  (concat (propertize title 'face 'transient-heading)
          (mapconcat (lambda (part) (concat "  " (propertize part 'face 'shadow)))
                     (delq nil parts) "")))

(defun r4-menus--tracks-heading ()
  "Return the heading of the menu of a list of tracks, and its track at point."
  (r4-menus--in-buffer
    (r4-menus--heading
     (concat "Tracks of " (r4-tracks--describe-source r4-tracks--source))
     (if r4-tracks--filter
         (format "%d of %d shown" (length tabulated-list-entries) (length r4-tracks--tracks))
       (format "%d tracks" (length r4-tracks--tracks)))
     (when-let* ((track (tabulated-list-get-id)))
       (concat "at point: " (r4-one-line (r4-track-title track)))))))

(defun r4-menus--channels-heading ()
  "Return the heading of the menu of a list of channels, and its channel at point."
  (r4-menus--in-buffer
    (r4-menus--heading
     (string-trim (buffer-name) "\\*r4 " "\\*")
     (format "%d channels" (length tabulated-list-entries))
     (when-let* ((channel (tabulated-list-get-id)))
       (concat "at point: @" (r4-channel-slug channel)
               (if-let* ((name (r4-channel-name channel))) (concat " " name) ""))))))

(defun r4-menus--queue-heading ()
  "Return the heading of the menu of the queue: the track at point."
  (r4-menus--in-buffer
    (r4-menus--heading
     "Queue"
     (format "%d tracks" (length r4-player-queue))
     (when-let* ((index (tabulated-list-get-id)))
       (concat "at point: " (r4-one-line (r4-track-title (aref r4-player-queue index))))))))

(defun r4-menus--channel-page-heading ()
  "Return the heading of the menu of a channel page: the channel."
  (r4-menus--in-buffer
    (r4-menus--heading
     (concat "Channel @" (r4-channel-slug r4-channel-page--channel))
     (r4-channel-name r4-channel-page--channel))))

(defun r4-menus--downloads-heading ()
  "Return the heading of the menu of the downloads: their counts."
  (r4-menus--in-buffer
    (r4-menus--heading
     "Downloads"
     (string-trim (r4-downloads--summary))
     (when-let* ((job (tabulated-list-get-id)))
       (concat "at point: " (r4-one-line (r4-track-title (r4-download-track job))))))))

(defun r4-menus--broadcasts-heading ()
  "Return the heading of the menu of the broadcasts."
  (r4-menus--in-buffer
    (r4-menus--heading
     "Live broadcasts"
     (format "%d live" (length tabulated-list-entries))
     (when r4-broadcast--listening
       (concat "listening to @" (plist-get r4-broadcast--listening :slug))))))

(defun r4-menus--on-off (value)
  "Return VALUE as on or off, highlighted when on."
  (if value (propertize "on" 'face 'transient-value) (propertize "off" 'face 'shadow)))

(defun r4-menus--shuffle ()
  "Return the description of the shuffle toggle."
  (concat "Shuffle " (r4-menus--on-off r4-player-shuffle)))

(defun r4-menus--video ()
  "Return the description of the video toggle."
  (concat "Video " (r4-menus--on-off r4-player-video)
          (propertize (if r4-player-video-pop-out
                          ", popped out (C-u V: back in)"
                        " (C-u V: pop out)")
                      'face 'shadow)))

(defun r4-menus--broadcast ()
  "Return the description of the broadcast toggle."
  (if r4-broadcasting
      (concat "Broadcasting as " (propertize (concat "@" (r4-channel-slug r4-broadcasting))
                                             'face 'transient-value))
    "Go live: broadcast"))

(defun r4-menus--player ()
  "Return the heading of the player group: the playing track."
  (concat (propertize "Player" 'face 'transient-heading) "  "
          (if-let* ((track (and (r4-player-live-p) (r4-player-current))))
              (format "%s %s  %s  %s" (if r4-player-paused "⏸" "▶")
                      (r4-one-line (r4-track-title track))
                      (r4-player-time-string)
                      (propertize (or r4-player-label (concat "@" (r4-track-slug track)))
                                  'face 'shadow))
            (propertize "nothing playing" 'face 'shadow))))

(defun r4-menus--queue-filter ()
  "Return the description of the filter of the queue."
  (if r4-queue-filter-text
      (format "Filter: %s" (propertize r4-queue-filter-text 'face 'transient-value))
    "Filter"))

(defun r4-menus--playing-p ()
  "Return non-nil if a track is playing."
  (and (r4-player-live-p) (r4-player-current)))

(defun r4-menus--local-list-p ()
  "Return non-nil in the list of a local channel."
  (r4-menus--in-buffer (plist-get r4-tracks--source :local)))

(defun r4-menus--remote-list-p ()
  "Return non-nil in a list of tracks of radio4000.com."
  (not (r4-menus--local-list-p)))

(defun r4-menus--playing-remote-p ()
  "Return non-nil if the playing track is on radio4000.com."
  (and (r4-menus--playing-p) (not (r4-track-local-p (r4-player-current)))))

(defun r4-menus--sync-label (slug)
  "Return the label of the sync toggle of the channel SLUG."
  (if (and slug (r4-synced-p slug))
      (concat "Sync channel " (r4-menus--on-off t))
    (concat "Sync channel " (r4-menus--on-off nil))))

(defun r4-menus--channel-sync ()
  "Return the label of the sync toggle of the channel at point."
  (r4-menus--sync-label (r4-menus--in-buffer
                          (when-let* ((channel (tabulated-list-get-id)))
                            (r4-channel-slug channel)))))

(defun r4-menus--page-sync ()
  "Return the label of the sync toggle of the channel of the page."
  (r4-menus--sync-label (r4-menus--in-buffer (r4-channel-slug r4-channel-page--channel))))

(defun r4-menus--list-sync ()
  "Return the label of the sync toggle of the channel of the list."
  (r4-menus--sync-label (r4-menus--in-buffer (plist-get r4-tracks--source :slug))))

(defun r4-menus--filter ()
  "Return the description of the filter of the track list."
  (if-let* ((text (r4-menus--in-buffer r4-tracks--filter-text)))
      (format "Filter: %s" (propertize text 'face 'transient-value))
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
      ("M-n" "Next track" r4-player-next :transient t)
      ("M-p" "Previous track" r4-player-previous :transient t)
      ("k" "Stop" r4-player-stop)]
     [("M-f" "Seek forward" r4-player-seek-forward :transient t)
      ("M-b" "Seek backward" r4-player-seek-backward :transient t)
      ("M-<up>" "Volume up" r4-player-volume-up :transient t)
      ("M-<down>" "Volume down" r4-player-volume-down :transient t)]
     [("Z" r4-player-toggle-shuffle :description r4-menus--shuffle :transient t)
      ("V" r4-player-toggle-video :description r4-menus--video :transient t)
      ("Q" "Queue" r4-queue)
      ("v" "Player window" r4-player-window)
      ("b" r4-broadcast-toggle :description r4-menus--broadcast)
      (";" "Share the playing track…" r4-player-share-menu :if r4-menus--playing-p)]]
    "The player group of the r4 menus.")

  (defconst r4-menus--go-to-group
    ["Go to, also without this menu"
     [("^" "Up, to home" r4-up)
      ("j" "Visit channel" r4-channel-page)]
     [("E" "Explore" r4-explore)
      ("B" "Broadcasts" r4-broadcasts)]
     [("H" "History" r4-history)
      ("." "All commands" r4-menu)]]
    "The group of the r4 menus going to the main views.
Their keys work in every r4 buffer, see `r4-player-map'."))

(defmacro r4-menus--define (name doc &rest groups)
  "Define the transient menu NAME with DOC, GROUPS, going to and the player.
The menu of all the commands, `r4-menu', has the main views in its
groups instead."
  (declare (indent 1) (doc-string 2))
  `(transient-define-prefix ,name ()
     ,doc
     ,@groups
     ,@(unless (eq name 'r4-menu) (list r4-menus--go-to-group))
     ,r4-menus--player-group))

;;;; Menus

;;;###autoload (autoload 'r4-menu "r4-menus" nil t)
(r4-menus--define r4-menu
  "Radio4000 menu, of all the commands."
  [["Channels"
    ("c" "Channels" r4-channels)
    ("j" "Visit channel" r4-channel-page)
    ("E" "Explore" r4-explore)
    ("t" "Channel tracks" r4-open-channel)
    ("@" "Mentions" r4-mentions)]
   ["Play"
    ("P" "Play channel" r4-play-channel)
    ("R" "Auto radio" r4-auto-radio)
    ("B" "Broadcasts" r4-broadcasts)]
   ["Tracks"
    ("s" "Search tracks: @slug #tag" r4-search)
    ("H" "History" r4-history)]
   ["Download"
    ("D" "Download channel" r4-download-channel)
    ("L" "Downloads" r4-downloads)
    ("F" "Downloads folder" r4-files-open-directory)
    ("y" "Sync channels now" r4-sync-channels)
    ("Y" "Synced channels" r4-list-synced-channels)]
   ["Local"
    ("l" "Local channels…" r4-local-menu)]
   ["Account"
    ("a" "Account…" r4-account-menu)]])

;;;###autoload (autoload 'r4-player-share-menu "r4-menus" nil t)
(transient-define-prefix r4-player-share-menu ()
  "Menu sharing the playing track."
  [:description r4-menus--player
   ["Share the playing track"
    ("w" "Copy media URL" r4-player-copy-url)
    ("W" "Copy radio4000.com URL" r4-player-copy-web-url :if r4-menus--playing-remote-p)
    ("o" "Open on radio4000.com" r4-player-browse-current :if r4-menus--playing-remote-p)
    ("O" "Open the media" r4-player-browse-media)]])

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
    ("e" "Edit channel" r4-edit-channel)
    ("X" "Delete channel" r4-delete-channel)]])

;;;###autoload (autoload 'r4-tracks-menu "r4-menus" nil t)
(r4-menus--define r4-tracks-menu
  "Menu of a list of tracks."
  [:description r4-menus--tracks-heading
   ["Play"
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
    ("t" "Toggle marks" r4-tracks-toggle-marks :transient t)
    ("% m" "Mark by regexp" r4-tracks-mark-regexp :transient t)
    ("U" "Unmark all" r4-tracks-unmark-all :transient t)]]
  [["Download"
    ("d" "Marked or at point" r4-tracks-download)
    ("D" "All" r4-tracks-download-all)
    ("F" "Visit file" r4-tracks-open-file)
    ("L" "Downloads" r4-downloads)
    ("y" r4-tracks-toggle-sync :description r4-menus--list-sync
     :if r4-menus--remote-list-p)]
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
  [:description r4-menus--channels-heading
   ["Channel"
    ("RET" "Visit channel" r4-channels-visit)
    ("T" "Tracks" r4-channels-open)
    ("P" "Play channel" r4-channels-play)
    ("z" "Shuffle" r4-channels-shuffle)
    ("R" "Auto radio" r4-channels-auto-radio)]
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
    ("y" r4-channels-toggle-sync :description r4-menus--channel-sync)
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
  [:description r4-menus--queue-heading
   ["Queue"
    ("RET" "Play track" r4-queue-play)
    ("x" "Remove" r4-queue-remove)
    ("C" "Clear" r4-player-clear)]
   ["Filter what plays"
    ("/" r4-queue-filter :description r4-menus--queue-filter)
    ("#" "Toggle tags" r4-queue-filter-tag)]
   ["Share"
    ("o" "Open on radio4000.com" r4-queue-browse)
    ("w" "Copy media URL" r4-queue-copy-url)
    ("W" "Copy radio4000.com URL" r4-queue-copy-web-url)]])

;;;###autoload (autoload 'r4-channel-page-menu "r4-menus" nil t)
(r4-menus--define r4-channel-page-menu
  "Menu of a channel page."
  [:description r4-menus--channel-page-heading
   ["Play"
    ("P" "Play" r4-channel-page-play)
    ("z" "Shuffle" r4-channel-page-shuffle)
    ("R" "Auto radio" r4-channel-page-auto-radio)]
   ["Pages"
    ("t" "Tracks" r4-channel-page-tracks)
    ("#" "Tracks with a tag" r4-channel-page-tag)
    ("F" "Following" r4-channel-page-following)
    ("r" "Followers" r4-channel-page-followers)
    ("M" "Mentions" r4-channel-page-mentions)]
   ["Channel"
    ("f" "Follow/unfollow" r4-channel-page-follow)
    ("y" r4-channel-page-toggle-sync :description r4-menus--page-sync)
    ("D" "Download" r4-channel-page-download)
    ("C" "Save locally" r4-channel-page-save-locally)]
   ["Share"
    ("o" "Open on radio4000.com" r4-channel-page-browse)
    ("w" "Copy radio4000.com URL" r4-channel-page-copy-url)
    ("g" "Refresh" revert-buffer)]])

;;;###autoload (autoload 'r4-downloads-menu "r4-menus" nil t)
(r4-menus--define r4-downloads-menu
  "Menu of the downloads."
  [:description r4-menus--downloads-heading
   ["At point"
    ("x" "Cancel" r4-downloads-cancel)
    ("r" "Retry" r4-downloads-retry)
    ("F" "Visit file" r4-downloads-visit-file)]
   ["All"
    ("R" "Retry failed" r4-downloads-retry-failed)
    ("c" "Clear finished" r4-downloads-clear)
    ("X" "Cancel all" r4-download-cancel-all)]
   ["Show"
    ("l" "Log" r4-downloads-show-log)
    ("o" "Downloads folder" r4-files-open-directory)
    ("g" "Refresh" revert-buffer)]])

;;;###autoload (autoload 'r4-broadcasts-menu "r4-menus" nil t)
(r4-menus--define r4-broadcasts-menu
  "Menu of the live broadcasts."
  [:description r4-menus--broadcasts-heading
   ["Listen"
    ("RET" "Listen to the broadcast at point" r4-broadcasts-listen)
    ("l" "Leave the broadcast" r4-broadcast-leave)
    ("g" "Refresh" revert-buffer)]])

;;;###autoload (autoload 'r4-player-menu "r4-menus" nil t)
(r4-menus--define r4-player-menu
  "Menu of the player.")

;; Like magit, the keys of the main views work in every r4 buffer.
(keymap-set r4-player-map "^" #'r4-up)
(keymap-set r4-player-map "H" #'r4-history)
(keymap-set r4-player-map "j" #'r4-channel-page)
(keymap-set r4-player-map "E" #'r4-explore)
(keymap-set r4-player-map "B" #'r4-broadcasts)
(keymap-set r4-player-map "b" #'r4-broadcast-toggle)

(keymap-set r4-tracks-mode-map "?" #'r4-tracks-menu)
(keymap-set r4-channels-mode-map "?" #'r4-channels-menu)
(keymap-set r4-queue-mode-map "?" #'r4-queue-menu)
(keymap-set r4-channel-page-mode-map "?" #'r4-channel-page-menu)
(keymap-set r4-downloads-mode-map "?" #'r4-downloads-menu)
(keymap-set r4-broadcasts-mode-map "?" #'r4-broadcasts-menu)
(keymap-set r4-player-window-mode-map "?" #'r4-player-menu)

(provide 'r4-menus)

;;; r4-menus.el ends here
