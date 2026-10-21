;;; r4-test-player.el --- Tests of r4: Player, queue, history, menus, auto-radio and broadcasts  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'r4-test-helpers)

(ert-deftest r4-test-player-advances-on-end-of-file ()
  (skip-unless (and (executable-find "mpv") (executable-find "ffmpeg")))
  (let* ((r4-download-directory (make-temp-file "r4-test" t))
         (directory (r4-files-tracks-directory "test"))
         (tracks (list (r4-test--track :id "11111111-a" :title "one" :provider "youtube" :media-id "1")
                       (r4-test--track :id "22222222-b" :title "two" :provider "youtube" :media-id "2")))
         (r4-player-arguments '("--ao=null"))
         (r4-player-shuffle nil))
    (unwind-protect
        (progn
          (make-directory directory t)
          (dolist (name '("one [1]" "two [2]"))
            (call-process "ffmpeg" nil nil nil "-loglevel" "quiet" "-f" "lavfi"
                          "-i" "sine=duration=0.5" (expand-file-name (concat name ".ogg") directory)))
          (r4-player-play tracks)
          (should (equal (r4-track-title (r4-player-current)) "one"))
          (with-timeout (10 (ert-fail "Player did not advance"))
            (while (equal (r4-track-title (r4-player-current)) "one")
              (accept-process-output nil 0.1)))
          (should (equal (r4-track-title (r4-player-current)) "two"))
          ;; The elapsed time and duration are read from mpv.
          (with-timeout (5 (ert-fail "No duration"))
            (while (not (r4-player-duration)) (accept-process-output nil 0.05)))
          (should (< 0.4 (r4-player-duration) 0.6))
          (should (equal (r4-player-time-string) "0:00/0:00"))
          ;; Start a track at a position, once loaded.
          (r4-player-play tracks 0 0.3 "test")
          (should (equal r4-player-label "test"))
          (with-timeout (5 (ert-fail "No seek"))
            (while r4-player--start-at (accept-process-output nil 0.05)))
          ;; Toggling video plays the track again from the same position.
          (let (reloaded)
            (cl-letf* ((play-index (symbol-function 'r4-player--play-index))
                       ((symbol-function 'r4-player--play-index)
                        (lambda (index &optional start)
                          (setq reloaded (list index start))
                          (funcall play-index index start))))
              (r4-player-toggle-video)
              (should r4-player-video)
              (with-timeout (5 (ert-fail "No reload"))
                (while (not reloaded) (accept-process-output nil 0.05)))
              (should (equal (car reloaded) r4-player-index))
              ;; From its position, or its start when between tracks.
              (should (or (null (cadr reloaded)) (numberp (cadr reloaded))))))
          (r4-player-toggle-video)
          (should-not r4-player-video)
          (r4-player-copy-url)
          (should (equal (current-kill 0) "https://www.youtube.com/watch?v=pmpXABDFMyE"))
          (r4-player-copy-web-url)
          (should (equal (current-kill 0) "https://radio4000.com/test/tracks/22222222-b")))
      (r4-player-stop)
      (delete-directory r4-download-directory t))))

(ert-deftest r4-test-player-queue ()
  (let ((r4-player-queue nil) (r4-player-index nil) (r4-player--history nil)
        (r4-player-queue-changed-hook nil)
        (one (r4-test--track :id "1" :title "one"))
        (two (r4-test--track :id "2" :title "two"))
        (three (r4-test--track :id "3" :title "three")))
    (r4-player-add-to-queue (list one two))
    (should (equal (mapcar #'r4-track-title r4-player-queue) '("one" "two")))
    (setq r4-player-index 0)
    (r4-player-play-next (list three))
    (should (equal (mapcar #'r4-track-title r4-player-queue) '("one" "three" "two")))
    (should-error (r4-player-remove 0) :type 'user-error)
    (setq r4-player-index 2)
    (r4-player-remove 1)
    (should (equal (mapcar #'r4-track-title r4-player-queue) '("one" "two")))
    ;; The index follows the playing track.
    (should (equal r4-player-index 1))
    (r4-player-clear)
    (should (equal (mapcar #'r4-track-title r4-player-queue) '("two")))
    (should (equal r4-player-index 0))))

;; Like radio4000.com, the filter of the queue decides what plays.
(ert-deftest r4-test-queue-filter ()
  (let ((r4-player-queue (vector (r4-test--track :id "1" :title "one" :tags '("jazz"))
                                 (r4-test--track :id "2" :title "two" :tags '("dub"))
                                 (r4-test--track :id "3" :title "three" :tags '("jazz"))
                                 (r4-test--track :id "4" :title "four" :tags '("dub"))))
        (r4-player-index 0) (r4-player--history nil) (r4-player-shuffle nil)
        (r4-player-label nil) (r4-player-next-function nil)
        (r4-player-queue-filter nil) (r4-queue-filter-text nil)
        played)
    (cl-letf (((symbol-function 'r4-player--play-index)
               (lambda (index &optional _) (setq played index r4-player-index index))))
      (r4-queue)
      (unwind-protect
          (with-current-buffer r4-queue-buffer
            (should (equal (length tabulated-list-entries) 4))
            (cl-letf (((symbol-function 'completing-read-multiple)
                       (lambda (&rest _) '("jazz"))))
              (call-interactively #'r4-queue-filter-tag))
            (should (equal r4-queue-filter-text "#jazz"))
            (should (equal (mapcar #'car tabulated-list-entries) '(0 2)))
            (should (string-match-p "#jazz 2/4" mode-line-process))
            (r4-player-next)
            (should (equal played 2))
            (r4-player-next)
            (should (equal played 0))
            (setq r4-player--history nil)
            (r4-player-previous)
            (should (equal played 2))
            ;; Shuffling plays among them too.
            (let ((r4-player-shuffle t))
              (dotimes (_ 5)
                (r4-player-next)
                (should (memq played '(0 2)))))
            (r4-queue-filter "")
            (should (equal (length tabulated-list-entries) 4))
            (let ((from r4-player-index))
              (r4-player-next)
              (should (equal played (mod (1+ from) 4))))
            (should-not mode-line-process))
        (kill-buffer r4-queue-buffer)))))

(ert-deftest r4-test-history ()
  (let* ((directory (make-temp-file "r4-test" t))
         ;; In a directory to create, like on a new computer.
         (r4-history-file (expand-file-name "new/r4-history.eld" directory))
         (r4-history--entries nil) (r4-history--loaded t)
         (one (r4-test--track :id "1" :title "one"))
         (r4-player-queue (vector one)) (r4-player-index 0))
    (unwind-protect
        (cl-letf (((symbol-function 'r4-player-live-p) #'always))
          (r4-history--record)
          (r4-history--record)
          (should (equal (length r4-history--entries) 1))
          (should (r4-history-played-p one))
          ;; It is saved, and loaded back.
          (setq r4-history--entries nil r4-history--loaded nil)
          (r4-history--load)
          (should (equal (r4-track-title (cdar r4-history--entries)) "one"))
          (r4-history)
          (with-current-buffer "*r4 history*"
            (should (equal (car (aref tabulated-list-format 1)) "time"))
            (should (equal (r4-track-title (tabulated-list-get-id (point-min))) "one")))
          (kill-buffer "*r4 history*"))
      (delete-directory directory t))))

(ert-deftest r4-test-mode-line ()
  (let ((r4-player-queue (vector (r4-test--track :title "Song")))
        (r4-player-index 0) (r4-player-paused nil) (r4-player-label "auto radio @x")
        (r4-player-time-pos nil) (r4-player--duration nil))
    (cl-letf (((symbol-function 'r4-player-live-p) #'always))
      (let ((string (r4-player-mode-line-string)))
        (should (string-match-p "r4 ▶ \\[auto radio @x\\] Song 0:00 " string))
        (should (eq (get-text-property 1 'local-map string) r4-player-mode-line-map))
        ;; A global minor mode shows it.
        (unwind-protect
            (progn (r4-player-mode-line-mode 1)
                   (should (memq 'r4-player--mode-line-construct global-mode-string)))
          (r4-player-mode-line-mode -1))
        (should-not (memq 'r4-player--mode-line-construct global-mode-string))
        (should (eq (keymap-lookup r4-player-mode-line-map "<mode-line> <mouse-1>")
                    'r4-player-toggle-pause))
        ;; With the elapsed time and the duration, from mpv or radio4000.com.
        (setq r4-player-time-pos 83.4)
        (setf (r4-track-duration (r4-player-current)) 296)
        (should (string-match-p "Song 1:23/4:56 " (r4-player-mode-line-string)))
        (setq r4-player--duration 3725.0)
        (should (equal (r4-player-time-string) "1:23/1:02:05"))))))

;; The player window shows the track, its time and a progress bar.
(ert-deftest r4-test-player-window ()
  (let ((r4-player-queue (vector (r4-test--track :title "Song" :slug "ko002")))
        (r4-player-index 0) (r4-player-paused nil) (r4-player-label nil)
        (r4-player-shuffle t) (r4-player-video nil)
        (r4-player-time-pos 30.0) (r4-player--duration 120.0)
        seeked)
    (cl-letf (((symbol-function 'r4-player-live-p) #'always)
              ((symbol-function 'r4-player-seek-percent)
               (lambda (percent) (setq seeked percent))))
      (unwind-protect
          (progn
            (r4-player-window)
            (let ((window (get-buffer-window r4-player-window-buffer)))
              (should (eq (window-parameter window 'window-side) 'top))
              (with-current-buffer r4-player-window-buffer
                (should (equal (count-lines (point-min) (point-max)) 3))
                (should (string-match-p "\\`▶ Song\n@ko002 0:30/2:00 ━+─+\n< previous  SPC pause"
                                        (buffer-string)))
                ;; The controls are buttons, shuffle shows it is on.
                (save-excursion
                  (search-forward "shuffle")
                  (should (eq (button-get (button-at (1- (point))) 'face) 'success)))
                ;; A quarter of the bar is played, clicking it seeks.
                (search-forward "━")
                (should (eq (get-text-property (point) 'keymap) r4-player-window-bar-map))
                (r4-player-window-seek-at-mouse `(mouse-1 (,window ,(point) (0 . 0) 0)))
                (should (< 0 seeked 5))
                ;; It follows the player.
                (setq r4-player-paused t r4-player-time-pos 60.0)
                (r4-player--changed)
                (should (string-match-p "\\`⏸ Song\n@ko002 1:00/2:00 .*\n< previous  SPC play"
                                        (buffer-string)))
                ;; While broadcasting, b stops it.
                (let ((r4-broadcasting (r4-channel-create :slug "ko002"))
                      (r4-player-changed-hook '(r4-player-window--refresh)))
                  (r4-player--changed)
                  (should (string-search "b stop live" (buffer-string))))))
            ;; v hides it.
            (r4-player-window)
            (should-not (get-buffer-window r4-player-window-buffer)))
        (kill-buffer r4-player-window-buffer)))))

(ert-deftest r4-test-video-window ()
  ;; Not in a terminal.
  (should-not (r4-video-window-p))
  (let ((r4-player-video t) (r4-video--frame 'frame) (r4-player-changed-hook nil)
        (r4-video-area-function #'ignore) quit)
    (cl-letf (((symbol-function 'frame-live-p) (lambda (frame) (eq frame 'frame)))
              ((symbol-function 'r4-video-quit)
               (lambda () (setq quit t r4-video--frame nil))))
      ;; Without the player window, videos turn off.
      (r4-video-place)
      (should quit)
      (should-not r4-player-video)))
  (should (eq (keymap-lookup r4-player-map "^") 'r4-player-toggle-video-pop-out))
  ;; The video is under the text of the player window.
  (let ((r4-player-queue (vector (r4-test--track :title "Song")))
        (r4-player-index 0) (r4-player-video nil))
    (cl-letf (((symbol-function 'r4-player-live-p) #'always))
      (unwind-protect
          (pcase-let* ((window (r4-player-window--show))
                       (`(,area-window ,_ ,top ,_ ,bottom) (r4-player-window--video-area)))
            (should (eq area-window window))
            (should (= top (+ (nth 1 (window-inside-pixel-edges window))
                              (* 3 (frame-char-height)))))
            (should (<= top bottom)))
        (kill-buffer r4-player-window-buffer)))))

;; Like the home page of radio4000.com.
(ert-deftest r4-test-home ()
  (let* ((track (r4-test--track :title "Song" :slug "ko002"))
         (r4-history--entries (list (cons "2026-10-21T10:00:00Z" track)))
         (r4-history--loaded t)
         (r4-auth--session nil)
         (r4-synced-channels '("oskar"))
         (r4-download-directory (make-temp-file "r4-test" t))
         visited)
    (cl-letf (((symbol-function 'r4-api-get)
               (lambda (_table _params callback)
                 (funcall callback '(((channel_id . "c1") (channels (slug . "live"))
                                      (deck (track_title . "Now")))))))
              ((symbol-function 'r4-channel-page) (lambda (slug) (setq visited slug))))
      (unwind-protect
          (progn
            (r4)
            (with-current-buffer r4-home-buffer
              (should (eq major-mode 'r4-home-mode))
              (dolist (text '("Nothing is playing" "Sign in" "@oskar" "@live" "Song"))
                (should (string-search text (buffer-string))))
              (goto-char (point-min))
              (search-forward "@oskar")
              (push-button (1- (point)))
              (should (equal visited "oskar"))
              (should (eq (keymap-lookup r4-home-mode-map "?") 'r4-menu))))
        (kill-buffer r4-home-buffer)
        (delete-directory r4-download-directory t)))))

;; The bar fits its width, also in fonts drawing its characters wider.
(ert-deftest r4-test-player-window-bar-width ()
  (let ((r4-player-queue (vector (r4-test--track))) (r4-player-index 0)
        (r4-player-time-pos 10.0) (r4-player--duration 20.0))
    (cl-letf (((symbol-function 'string-pixel-width)
               (lambda (string &rest _) (if (equal string " ") 8 16))))
      (should (equal (length (r4-player-window--bar 40)) 20)))
    (cl-letf (((symbol-function 'string-pixel-width) (lambda (&rest _) 8)))
      (should (equal (length (r4-player-window--bar 40)) 40)))))

;; Like the live button of radio4000.com: a row of the broadcast table,
;; and state messages through Supabase Realtime.
(ert-deftest r4-test-broadcasting ()
  (let* ((track (r4-test--track :id "11111111-1111-1111-1111-111111111111" :slug "ko002"))
         (r4-player-queue (vector track)) (r4-player-index 0)
         (r4-player-paused nil) (r4-player-time-pos 30.0)
         (r4-follows--my-channel (r4-channel-create :id "c1" :slug "ko002"))
         (r4-broadcasting nil) (r4-broadcasting--seq 0) (r4-player-mode-line-extra nil)
         sent deleted)
    (cl-letf (((symbol-function 'r4-player-live-p) #'always)
              ((symbol-function 'r4-auth-require) #'ignore)
              ((symbol-function 'r4-auth-token) (lambda () "token-1"))
              ((symbol-function 'r4-api-send)
               (lambda (method path &rest args) (push (list method path args) sent)))
              ((symbol-function 'r4-api-request)
               (lambda (method path &rest args) (push (list method path args) deleted))))
      (unwind-protect
          (progn
            (r4-broadcast-start)
            (should (equal r4-player-mode-line-extra "live"))
            (pcase-let ((`(,table ,realtime) sent))
              (should (equal (nth 1 realtime) "/realtime/v1/api/broadcast"))
              (let* ((message (aref (alist-get 'messages (plist-get (nth 2 realtime) :body)) 0))
                     (deck (aref (alist-get 'decks (alist-get 'payload message)) 0)))
                (should (equal (alist-get 'topic message) "broadcast-state:c1"))
                (should (equal (alist-get 'event message) "state"))
                (should (equal (alist-get 'track_id deck) (r4-track-id track)))
                (should (equal (alist-get 'seek_position deck) 30.0))
                (should (eq (alist-get 'is_playing deck) t)))
              (should (equal (nth 1 table) "/rest/v1/broadcast"))
              (should (equal (alist-get 'channel_id (plist-get (nth 2 table) :body)) "c1")))
            ;; Nothing changed, nothing is sent.
            (setq sent nil)
            (r4-player--changed)
            (should-not sent)
            ;; A pause is sent.
            (setq r4-player-paused t)
            (r4-player--changed)
            (should (eq (alist-get 'is_playing
                                   (aref (alist-get 'decks
                                                    (alist-get 'payload
                                                               (aref (alist-get 'messages (plist-get (nth 2 (cadr sent)) :body)) 0)))
                                         0))
                        :false))
            ;; A seek is sent.
            (setq sent nil r4-player-time-pos 100.0)
            (r4-player--changed)
            (should sent)
            ;; Local tracks come with their URL.
            (let ((local (r4-test--track :id "x" :url "https://youtu.be/abc" :title "Local")))
              (r4-track-mark-local local)
              (should (equal (alist-get 'track_url (r4-broadcasting-deck local 0 t))
                             "https://youtu.be/abc")))
            (r4-broadcast-stop)
            (should (equal (nth 1 (car deleted)) "/rest/v1/broadcast"))
            (should-not r4-broadcasting)
            (should-not r4-player-mode-line-extra))
        (r4-broadcast-stop)))))

;; Like magit, each buffer has its menu, saying what it acts on.
(ert-deftest r4-test-menus-context ()
  (dolist (map-menu `((,r4-tracks-mode-map . r4-tracks-menu)
                      (,r4-channels-mode-map . r4-channels-menu)
                      (,r4-queue-mode-map . r4-queue-menu)
                      (,r4-channel-page-mode-map . r4-channel-page-menu)
                      (,r4-downloads-mode-map . r4-downloads-menu)
                      (,r4-broadcasts-mode-map . r4-broadcasts-menu)
                      (,r4-player-window-mode-map . r4-player-menu)
                      (,r4-home-mode-map . r4-menu)))
    (should (eq (keymap-lookup (car map-menu) "?") (cdr map-menu))))
  (let ((buffer (generate-new-buffer "r4-test")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (r4-tracks-mode)
            (setq r4-tracks--source '(:slug "ko002"))
            (r4-tracks--set buffer (list (r4-test--track :title "Song" :tags '("jazz"))
                                         (r4-test--track :id "2" :title "Other")))
            (r4-tracks-filter "#jazz")
            (goto-char (point-min)))
          ;; Descriptions are formatted in another buffer.
          (let ((transient--original-buffer buffer))
            (with-temp-buffer
              (let ((heading (r4-menus--tracks-heading)))
                (should (string-match-p "Tracks of @ko002" heading))
                (should (string-match-p "1 of 2 shown" heading))
                (should (string-match-p "at point: Song" heading)))
              (should (string-match-p "#jazz" (r4-menus--filter))))))
      (kill-buffer buffer))))

;; The video is beside the player in a wide window, else under it, at
;; its aspect ratio.
(ert-deftest r4-test-player-window-video-layout ()
  (let ((r4-player-video t) (r4-player-video-aspect 1.0) (width 1900)
        (r4-player-window-text-width 70))
    (cl-letf (((symbol-function 'r4-video-window-p) #'always)
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-frame) #'ignore)
              ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
              ((symbol-function 'window-body-width) (lambda (&rest _) width))
              ((symbol-function 'window-body-height) (lambda (&rest _) 400))
              ((symbol-function 'window-inside-pixel-edges)
               (lambda (&rest _) (list 10 0 (+ 10 width) 400)))
              ((symbol-function 'window-text-pixel-size) (lambda (&rest _) '(700 . 60))))
      ;; A square video, 400 pixels high, at the right.
      (should (r4-player-window--beside-p 'window))
      (should (equal (r4-player-window--video-area) (list 'window 1510 0 1910 400)))
      ;; Too narrow: under the player, centered.
      (setq width 900)
      (should-not (r4-player-window--beside-p 'window))
      (should (equal (r4-player-window--video-area) (list 'window 290 60 630 400))))))

(ert-deftest r4-test-menus ()
  (dolist (menu '(r4-menu r4-tracks-menu r4-channels-menu r4-queue-menu))
    (should (commandp menu)))
  (should (eq (keymap-lookup r4-tracks-mode-map "?") 'r4-tracks-menu))
  ;; The player keys work in every list.
  (dolist (map (list r4-tracks-mode-map r4-channels-mode-map r4-queue-mode-map
                     r4-downloads-mode-map r4-broadcasts-mode-map r4-home-mode-map
                     r4-channel-page-mode-map r4-player-window-mode-map))
    (should (eq (keymap-lookup map ">") 'r4-player-next))
    ;; Like magit, the main views are a key away in every r4 buffer.
    (should (eq (keymap-lookup map "h") 'r4))
    (should (eq (keymap-lookup map "j") 'r4-channel-page)))
  (dolist (map (list r4-tracks-mode-map r4-channels-mode-map r4-queue-mode-map))
    (should (eq (keymap-lookup map "S") 'tabulated-list-sort))
    (should (eq (keymap-lookup map "n") 'next-line)))
  ;; No list key hides a player key, and no menu has the same key twice.
  (dolist (map (list r4-tracks-mode-map r4-channels-mode-map r4-queue-mode-map))
    (map-keymap (lambda (key _)
                  (when-let* ((player (lookup-key r4-player-map (vector key))))
                    (should (eq player (lookup-key map (vector key))))))
                map))
  (dolist (menu '(r4-menu r4-local-menu r4-account-menu r4-tracks-menu r4-channels-menu r4-queue-menu
                  r4-channel-page-menu r4-downloads-menu r4-broadcasts-menu r4-player-menu))
    (let (keys)
      ;; Find the :key of the suffixes, in any version of transient.
      (cl-labels ((collect (x)
                    (cond ((and (consp x) (eq (car x) :key) (stringp (cadr x)))
                           (push (cadr x) keys)
                           (collect (cddr x)))
                          ((consp x) (collect (car x)) (collect (cdr x)))
                          ((vectorp x) (mapc #'collect x)))))
        (collect (get menu 'transient--layout)))
      (should (> (length keys) 10))
      (should (equal (length keys) (length (delete-dups (copy-sequence keys))))))))

(ert-deftest r4-test-auto-radio ()
  ;; Expected values computed by src/lib/player/auto-radio.ts of
  ;; radio4000.com, so everyone hears the same track at the same second.
  (let* ((tracks (mapcar (lambda (spec)
                           (r4-test--track :id (nth 0 spec) :created-at (nth 1 spec)
                                           :duration (nth 2 spec)))
                         '(("e1" "2020-01-01T00:00:00.000+00:00" 200)
                           ("a2" "2019-06-15T10:20:30.123+00:00" 150)
                           ("c3" "2021-03-03T03:03:03+00:00" 300)
                           ("b4" "2022-02-02T02:02:02+00:00" 95)
                           ("d5" "2023-05-05T05:05:05+00:00" 410)
                           ("f6" "2024-01-01T00:00:00+00:00" 0))))
         (playable (r4-auto-radio-tracks tracks))
         (epoch (r4-auto-radio-epoch playable)))
    (should (equal (length playable) 5))
    (pcase-dolist (`(,now ,seed ,ids ,index ,offset)
                   '((1792530000000 "@test" ("b4" "e1" "d5" "c3" "a2") 2 125)
                     (1792530000000 nil ("b4" "a2" "e1" "c3" "d5") 2 175)
                     (1700000123456 "@test" ("b4" "a2" "e1" "c3" "d5") 4 313.456)))
      (let* ((schedule (r4-auto-radio-schedule playable epoch now seed))
             (position (r4-auto-radio-position schedule epoch now)))
        (should (equal (mapcar #'r4-track-id schedule) ids))
        (should (equal (car position) index))
        (should (< (abs (- (cdr position) offset)) 0.001))))
    (should (equal (r4-auto-radio--hash-string "@oskar") 2772611237))))

(ert-deftest r4-test-broadcast-position ()
  ;; Like calculateSeekTime of radio4000.com.
  (let* ((now 1000000.0)
         (ago (lambda (seconds) (format-time-string "%FT%T.%3NZ" (- now seconds) t))))
    (should (equal (r4-broadcast-position
                    `((seek_position . 10) (seeked_at . ,(funcall ago 5)) (is_playing . t))
                    nil now)
                   15))
    (should (equal (r4-broadcast-position
                    `((seek_position . 10) (seeked_at . ,(funcall ago 5)) (is_playing . nil))
                    nil now)
                   10))
    (should (equal (r4-broadcast-position
                    `((track_played_at . ,(funcall ago 30)) (speed . 2)) nil now)
                   60))
    (should-not (r4-broadcast-position `((track_played_at . ,(funcall ago 300))) 200 now))
    (should-not (r4-broadcast-position `((track_played_at . ,(funcall ago -10))) nil now))
    ;; The first deck of the broadcaster wins over the row.
    (should (equal (r4-broadcast-deck '((track_id . "row") (decks ((track_id . "deck")))))
                   '((track_id . "deck"))))))

(ert-deftest r4-test-broadcast-listen ()
  (let ((r4-broadcast--listening nil) (r4-broadcast--timer nil)
        (r4-player-label nil) (r4-player-index nil) (r4-player-next-function nil)
        (row `((channel_id . "c1") (track_id . "t1")
               (track_played_at . ,(format-time-string "%FT%T.%3NZ" (- (float-time) 42) t))))
        played)
    (cl-letf (((symbol-function 'run-with-timer) #'ignore)
              ((symbol-function 'r4-broadcast--fetch) (lambda (_id callback) (funcall callback row)))
              ((symbol-function 'r4-broadcast--fetch-track)
               (lambda (id callback) (funcall callback (r4-test--track :id id :duration 300))))
              ((symbol-function 'r4-player-live-p) #'ignore)
              ((symbol-function 'r4-player-play)
               (lambda (tracks _index start label)
                 (setq played (list (r4-track-id (car tracks)) start label)
                       r4-player-label label r4-player-index 0))))
      (r4-broadcast-listen "c1" "dj")
      (r4-broadcast--check)
      (should (equal (car played) "t1"))
      (should (<= 42 (nth 1 played) 43))
      (should (equal (nth 2 played) "broadcast @dj"))
      (should (eq r4-player-next-function #'r4-broadcast--next))
      ;; Playing something else leaves the broadcast.
      (setq r4-player-label nil)
      (r4-broadcast--check)
      (should-not r4-broadcast--listening)
      ;; So does the end of the broadcast.
      (r4-broadcast-listen "c1" "dj")
      (setq row nil)
      (r4-broadcast--check)
      (should-not r4-broadcast--listening))))

(ert-deftest r4-test-mpv-environment ()
  (let ((process-environment '("LIBGL_ALWAYS_SOFTWARE=1" "HOME=/home/x" "LANG=C")))
    (should (equal (r4-mpv--environment '("LIBGL_ALWAYS_SOFTWARE" "LANG=fr_FR.UTF-8"))
                   '("LANG=fr_FR.UTF-8" "HOME=/home/x")))
    (should (equal (r4-mpv--environment nil) process-environment))))

(provide 'r4-test-player)

;;; r4-test-player.el ends here
