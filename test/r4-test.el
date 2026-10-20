;;; r4-test.el --- Tests for r4  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'ert)
(require 'r4)

(defun r4-test--track (&rest fields)
  "Return an `r4-track' with FIELDS and default values."
  (apply #'r4-track-create
         (append fields (list :id "9919e967-db35-459b-b3dd-b9f439d4b827"
                              :slug "test" :title "A track"
                              :url "https://www.youtube.com/watch?v=pmpXABDFMyE"))))

(ert-deftest r4-test-track-from-json ()
  (let ((track (r4-track-from-json
                '((id . "abc") (slug . "oskar") (title . "Song") (media_id . "xyz")
                  (tags . ("a" "b")) (duration . 213) (created_at . "2026-10-18T12:13:17Z")))))
    (should (equal (r4-track-media-id track) "xyz"))
    (should (equal (r4-track-tags track) '("a" "b")))
    (should (equal (r4-track-web-url track) "https://radio4000.com/oskar/tracks/abc"))))

(ert-deftest r4-test-channel-from-json ()
  (let ((channel (r4-channel-from-json '((slug . "oskar") (track_count . 2648)))))
    (should (equal (r4-channel-track-count channel) 2648))
    (should (equal (r4-channel-web-url channel) "https://radio4000.com/oskar"))))

(ert-deftest r4-test-format ()
  (should (equal (r4-format-duration 213) "3:33"))
  (should (equal (r4-format-duration 3725) "1:02:05"))
  (should (equal (r4-format-duration nil) ""))
  (should (equal (r4-format-date "2026-10-18T12:13:17Z") "2026-10-18"))
  (should (equal (r4-one-line " a\n\tb ") "a b")))

(ert-deftest r4-test-api-params ()
  (should (equal (assoc "fts" (r4-api--channels-params "jazz blues"))
                 '("fts" "wfts.jazz blues")))
  (should-not (assoc "fts" (r4-api--channels-params)))
  (should (string-match-p "fts=wfts.jazz%20blues"
                          (r4-api--url "x" (r4-api--channels-params "jazz blues")))))

(ert-deftest r4-test-download-basename ()
  (should (equal (r4-download-basename (r4-test--track :title "AC/DC: \"Live\"" :media-id "xyz"))
                 "AC_DC_ _Live_ [xyz]"))
  (should (equal (r4-download-basename (r4-test--track :title "  "))
                 "untitled [9919e967]"))
  (should (<= (string-bytes (r4-download--sanitize (make-string 300 ?é))) 180))
  ;; Used to hang, see `r4-download--sanitize'.
  (should (equal (r4-download--sanitize "Molam International Band - ลำสั้นดิสโก้")
                 "Molam International Band - ลำสั้นดิสโก้"))
  (should (<= (string-bytes (r4-download--sanitize (apply #'concat (make-list 50 "ลำสั้น")))) 180))
  (should (equal (r4-download--sanitize "Shekarchi (شکارچی) - توله") "Shekarchi (شکارچی) - توله")))

(ert-deftest r4-test-download-command-escapes-percent ()
  (let ((r4-download-directory "/music/")
        (r4-download-audio-format "flac"))
    (let ((command (r4-download--command (r4-test--track :title "100% hits" :media-id "m"))))
      (should (member "/music/test/100%% hits [m].%(ext)s" command))
      (should (member "flac" command))
      (should (equal (car (last command)) "https://www.youtube.com/watch?v=pmpXABDFMyE")))))

(ert-deftest r4-test-download-index ()
  (let* ((r4-download-directory (make-temp-file "r4-test" t))
         (track (r4-test--track :media-id "m"))
         (directory (r4-download-channel-directory "test")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (should-not (r4-download-file track))
          (write-region "" nil (expand-file-name "A track [m].webp" directory))
          (write-region "" nil (expand-file-name "A track [m].opus.part" directory))
          (should-not (r4-download-file track))
          (write-region "" nil (expand-file-name "A track [m].opus" directory))
          (should (equal (file-name-nondirectory (r4-download-file track)) "A track [m].opus"))
          (puthash "test" (list track) r4-download--playlists)
          (r4-download-write-playlist "test")
          (should (equal (with-temp-buffer
                           (insert-file-contents (expand-file-name "test.m3u" directory))
                           (buffer-string))
                         "#EXTM3U\n#EXTINF:-1,A track\nA track [m].opus\n")))
      (delete-directory r4-download-directory t))))

(ert-deftest r4-test-player-advances-on-end-of-file ()
  (skip-unless (and (executable-find "mpv") (executable-find "ffmpeg")))
  (let* ((r4-download-directory (make-temp-file "r4-test" t))
         (directory (r4-download-channel-directory "test"))
         (tracks (list (r4-test--track :id "11111111-a" :title "one" :media-id "1")
                       (r4-test--track :id "22222222-b" :title "two" :media-id "2")))
         (r4-player-arguments '("--no-video" "--ao=null"))
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
          (should (equal (r4-track-title (r4-player-current)) "two")))
      (r4-player-stop)
      (delete-directory r4-download-directory t))))

(ert-deftest r4-test-tracks-buffer ()
  (let ((buffer (generate-new-buffer "r4-test")))
    (unwind-protect
        (with-current-buffer buffer
          (r4-tracks-mode)
          (r4-tracks--set buffer (list (r4-test--track :duration 213 :tags '("x"))))
          (goto-char (point-min))
          (should (equal (r4-track-title (tabulated-list-get-id)) "A track"))
          (should (search-forward "3:33" nil t))
          (r4-tracks-mark)
          (goto-char (point-min))
          (should (equal (length (r4-tracks--marked)) 1)))
      (kill-buffer buffer))))

(ert-deftest r4-test-local-helpers ()
  (should (equal (r4-local-slugify "  Ma Radio: Été!! ") "ma-radio-été"))
  (should (equal (r4-local-media-id "https://youtu.be/pmpXABDFMyE?t=3") "pmpXABDFMyE"))
  (should (equal (r4-local-media-id "https://www.youtube.com/watch?v=pmpXABDFMyE&x=1") "pmpXABDFMyE"))
  (should-not (r4-local-media-id "https://soundcloud.com/a/b"))
  (should (equal (r4-local-tags "nice #Jazz and #cover #jazz") '("jazz" "cover")))
  (should (string-match-p "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\'"
                          (r4-local-uuid))))

(ert-deftest r4-test-local-channel ()
  (let ((r4-download-directory (make-temp-file "r4-test" t)))
    (unwind-protect
        (progn
          (r4-local-create-channel "My Radio" "Hello — été")
          (should (equal (r4-local-slugs) '("my-radio")))
          (should-error (r4-local-create-channel "my radio"))
          (r4-local-add-track "my-radio" "https://youtu.be/pmpXABDFMyE" "Song" "a #tag")
          (r4-local--add "my-radio" (list (r4-test--track :title "Copy" :tags '("x"))))
          (let* ((local (r4-local-read "my-radio"))
                 (tracks (cdr local)))
            (should (equal (r4-channel-name (car local)) "My Radio"))
            (should (equal (r4-channel-description (car local)) "Hello — été"))
            (should (equal (r4-channel-track-count (car local)) 2))
            (should (equal (mapcar #'r4-track-title tracks) '("Copy" "Song")))
            (should (equal (r4-track-slug (car tracks)) "my-radio"))
            (should (equal (r4-track-tags (cadr tracks)) '("tag")))
            (should (equal (r4-track-media-id (cadr tracks)) "pmpXABDFMyE")))
          ;; The file is a radio4000.com backup (version 2).
          (let ((data (with-temp-buffer
                        (insert-file-contents (r4-local-file "my-radio"))
                        (json-parse-buffer :object-type 'alist :null-object nil))))
            (should (equal (alist-get 'version data) 2))
            (should (alist-get 'id (alist-get 'channel data)))
            (should (equal (length (alist-get 'tracks data)) 2))))
      (delete-directory r4-download-directory t))))

;;;; Sync, against a fake Supabase backend

(defvar r4-test--db nil "Fake database: alist of table name to rows.")
(defvar r4-test--requests nil "Requests received by the fake backend.")

(defun r4-test--param (params key)
  "Return the value of KEY in PARAMS, without its eq. prefix."
  (when-let* ((value (cadr (assoc key params))))
    (string-remove-prefix "eq." value)))

(cl-defun r4-test--fake-request (method path &key params body token _prefer)
  "Answer the request METHOD PATH like Supabase, from `r4-test--db'."
  (push (list method path params body token) r4-test--requests)
  (pcase (list method path)
    (`("POST" "/auth/v1/token?grant_type=password")
     (if (equal (alist-get 'password body) "secret")
         '((access_token . "token-1") (refresh_token . "refresh-1") (expires_in . 3600)
           (user (id . "user-1") (email . "me@example.org")))
       (signal 'r4-api-error '("Invalid login credentials"))))
    (`("POST" "/auth/v1/token?grant_type=refresh_token")
     (if (equal (alist-get 'refresh_token body) "refresh-1")
         '((access_token . "token-1") (refresh_token . "refresh-2") (expires_in . 3600)
           (user (id . "user-1") (email . "me@example.org")))
       (signal 'r4-api-error '("Refresh token is not valid"))))
    (`("POST" "/auth/v1/logout") nil)
    (`("GET" ,(rx "/rest/v1/" (let table (+ (not "?")))))
     (unless (equal token "token-1") (signal 'r4-api-error '("unauthorized")))
     (let ((rows (alist-get (intern table) r4-test--db)))
       (dolist (column '("id" "slug" "channel_id" "user_id"))
         (when-let* ((value (r4-test--param params column)))
           (setq rows (cl-remove-if-not
                       (lambda (row) (equal (alist-get (intern column) row) value))
                       rows))))
       rows))
    (`("POST" ,(rx "/rest/v1/" (let table (+ anychar))))
     (unless (equal token "token-1") (signal 'r4-api-error '("unauthorized")))
     (let ((key (intern table)))
       (setf (alist-get key r4-test--db)
             (append (alist-get key r4-test--db)
                     (if (vectorp body) (append body nil) (list body)))))
     nil)))

(defmacro r4-test--with-backend (&rest body)
  "Run BODY with a fake backend, a temporary music directory and no prompts."
  (declare (indent 0))
  `(let ((r4-download-directory (make-temp-file "r4-test" t))
         (r4-auth-session-file nil)
         (r4-auth--session nil)
         (r4-test--db nil)
         (r4-test--requests nil))
     (cl-letf (((symbol-function 'r4-api-request) #'r4-test--fake-request)
               ((symbol-function 'yes-or-no-p) #'always))
       (unwind-protect (progn (r4-login-with-password "me@example.org" "secret") ,@body)
         (delete-directory r4-download-directory t)))))

(ert-deftest r4-test-auth ()
  (r4-test--with-backend
    (should (equal (r4-auth-user-id) "user-1"))
    (should (equal (r4-auth-token) "token-1"))
    (should-error (r4-login-with-password "me@example.org" "wrong") :type 'r4-api-error)
    ;; An expired session is refreshed.
    (setf (plist-get r4-auth--session :expires-at) 0)
    (should (equal (r4-auth-token) "token-1"))
    (should (equal (plist-get r4-auth--session :refresh-token) "refresh-2"))
    ;; The session is saved, readable only by the user, and loaded again.
    (let ((r4-auth-session-file (expand-file-name "session.eld" r4-download-directory)))
      (r4-auth--save)
      (should (equal (file-modes r4-auth-session-file) #o600))
      (let ((saved r4-auth--session))
        (setq r4-auth--session nil)
        (should (equal (r4-auth-session) saved))))
    (r4-logout)
    (should-not (r4-auth-session))))

(ert-deftest r4-test-sync-publishes-then-merges ()
  (r4-test--with-backend
    (let* ((channel (r4-local-create-channel "My Radio"))
           (id (r4-channel-id channel)))
      (r4-local-add-track "my-radio" "https://youtu.be/aaaaaaaaaaa" "One" "")
      (r4-local-add-track "my-radio" "https://youtu.be/bbbbbbbbbbb" "Two" "#tag")
      ;; First sync publishes the channel and its tracks.
      (r4-sync-channel "my-radio")
      (should (equal (alist-get 'id (car (alist-get 'channels r4-test--db))) id))
      (should (equal (alist-get 'user_channel r4-test--db)
                     `(((user_id . "user-1") (channel_id . ,id)))))
      (should (equal (mapcar (lambda (row) (alist-get 'title row)) (alist-get 'tracks r4-test--db))
                     '("Two" "One")))
      (should (cl-every (lambda (row) (equal (alist-get 'channel_id row) id))
                        (alist-get 'channel_track r4-test--db)))
      ;; Every write is authenticated.
      (should (cl-every (lambda (request) (equal (nth 4 request) "token-1"))
                        (cl-remove-if (lambda (request) (string-prefix-p "/auth" (nth 1 request)))
                                      r4-test--requests)))
      ;; A track added on radio4000.com is downloaded, a local one uploaded.
      (push '((id . "remote-track") (slug . "my-radio") (title . "Remote")
              (url . "https://youtu.be/ccccccccccc") (created_at . "2000-01-01T00:00:00Z"))
            (alist-get 'channel_tracks r4-test--db))
      (dolist (row (alist-get 'tracks r4-test--db))
        (push (append row '((slug . "my-radio"))) (alist-get 'channel_tracks r4-test--db)))
      (r4-local-add-track "my-radio" "https://youtu.be/ddddddddddd" "Three" "")
      (setq r4-test--requests nil)
      (r4-sync-channel "my-radio")
      (should-not (cl-find "channels" r4-test--requests
                           :key (lambda (r) (and (equal (car r) "POST") (string-remove-prefix "/rest/v1/" (nth 1 r))))
                           :test #'equal))
      (should (equal (mapcar (lambda (row) (alist-get 'title row)) (alist-get 'tracks r4-test--db))
                     '("Two" "One" "Three")))
      (should (equal (mapcar #'r4-track-title (cdr (r4-local-read "my-radio")))
                     '("Three" "Two" "One" "Remote"))))))

(ert-deftest r4-test-sync-refuses-other-accounts ()
  (r4-test--with-backend
    (let ((channel (r4-local-create-channel "Taken")))
      ;; Same slug, owned by someone else.
      (push `((id . "other") (slug . "taken")) (alist-get 'channels r4-test--db))
      (should-error (r4-sync-channel "taken") :type 'user-error)
      ;; A saved copy of someone else's channel.
      (push `((id . ,(r4-channel-id channel)) (slug . "taken")) (alist-get 'channels r4-test--db))
      (should-error (r4-sync-channel "taken") :type 'user-error)
      (should-not (alist-get 'tracks r4-test--db)))))

(ert-deftest r4-test-sync-links-offline-channel ()
  (r4-test--with-backend
    (r4-local-create-channel "Mine")
    (push '((id . "remote-id") (slug . "mine")) (alist-get 'channels r4-test--db))
    (push '((user_id . "user-1") (channel_id . "remote-id")) (alist-get 'user_channel r4-test--db))
    (r4-local-add-track "mine" "https://youtu.be/aaaaaaaaaaa" "One" "")
    (r4-sync-channel "mine")
    (should (equal (r4-channel-id (car (r4-local-read "mine"))) "remote-id"))
    (should (equal (alist-get 'channel_id (car (alist-get 'channel_track r4-test--db)))
                   "remote-id"))))

(provide 'r4-test)

;;; r4-test.el ends here
