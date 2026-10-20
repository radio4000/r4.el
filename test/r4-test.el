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
  (should (equal (r4-format-duration 59.7) "0:59"))
  (should (equal (r4-format-duration nil) ""))
  (should (equal (r4-format-date "2026-10-18T12:13:17Z") "2026-10-18"))
  (should (equal (r4-one-line " a\n\tb ") "a b")))

(ert-deftest r4-test-api-params ()
  (should (equal (assoc "or" (r4-api--channels-params "jazz blues"))
                 '("or" "(fts.wfts.jazz blues,fts.fts.jazz:* & blues:*)")))
  (should-not (assoc "or" (r4-api--channels-params)))
  ;; Emacs versions escape different characters, so check the decoded
  ;; value, and that its & does not split the query.
  (let ((url (r4-api--url "x" (r4-api--channels-params "jazz blues"))))
    (should (string-match "[?&]or=\\([^&]*\\)" url))
    (should (equal (url-unhex-string (match-string 1 url))
                   "(fts.wfts.jazz blues,fts.fts.jazz:* & blues:*)"))))

(ert-deftest r4-test-search-filter ()
  (should (equal (r4-api-search-filter "osk") "(fts.wfts.osk,fts.fts.osk:*)"))
  ;; Web search operators disable the prefix search.
  (should (equal (r4-api-search-filter "jazz -blues") "(fts.wfts.jazz -blues)"))
  (should (equal (r4-api-search-filter "jazz or blues") "(fts.wfts.jazz or blues)"))
  ;; Characters breaking the PostgREST syntax are removed.
  (should (equal (r4-api-search-filter "a,b(c)") "(fts.wfts.a b c,fts.fts.a:* & b:* & c:*)"))
  (should-not (r4-api-search-filter " , ")))

(ert-deftest r4-test-search-parse ()
  (should (equal (r4-search-parse "@ko002 #Jazz miles  davis @Oskar")
                 '(:slugs ("ko002" "oskar") :tags ("jazz") :query "miles davis")))
  (should-not (r4-search-parse "  "))
  (should (equal (r4-api--tracks-params '(:slugs ("a" "b") :tags ("jazz" "dub") :mention "x"))
                 `(("select" ,r4-api--track-columns)
                   ("slug" "in.(a,b)")
                   ("tags" "ov.{\"jazz\",\"dub\"}")
                   ("description" "ilike.*@x*")
                   ("order" "created_at.desc")
                   ("limit" "5000")))))

(ert-deftest r4-test-tracks-filter ()
  (let ((buffer (generate-new-buffer "r4-test"))
        (tracks (list (r4-test--track :id "1" :title "Miles Davis - So What" :tags '("jazz"))
                      (r4-test--track :id "2" :title "King Tubby - Dub" :tags '("dub" "jazz"))
                      (r4-test--track :id "3" :title "Other" :slug "else"))))
    (unwind-protect
        (with-current-buffer buffer
          (r4-tracks-mode)
          (r4-tracks--set buffer tracks)
          (r4-tracks-filter "#jazz")
          (should (equal (length tabulated-list-entries) 2))
          (should (equal mode-line-process " 2/3"))
          (r4-tracks-filter "#jazz miles")
          (should (equal (mapcar (lambda (e) (r4-track-id (car e))) tabulated-list-entries) '("1")))
          (r4-tracks-filter "@else")
          (should (equal (mapcar (lambda (e) (r4-track-id (car e))) tabulated-list-entries) '("3")))
          (r4-tracks-filter "")
          (should (equal (length tabulated-list-entries) 3))
          (should-not mode-line-process)
          (should (equal (r4-search-tag-counts tracks) '(("jazz" . 2) ("dub" . 1)))))
      (kill-buffer buffer))))

(ert-deftest r4-test-columns-sort ()
  (let* ((specs r4-channels-column-specs)
         (format (r4-columns-list-format '(slug tracks) specs))
         (sort-tracks (nth 2 (aref format 1)))
         (entry (lambda (count) (list (r4-channel-create :slug "x" :track-count count) nil))))
    ;; Numbers sort by value, not by text.
    (should (funcall sort-tracks (funcall entry 9) (funcall entry 10)))
    (should-not (funcall sort-tracks (funcall entry 10) (funcall entry 9)))
    (should (funcall sort-tracks (funcall entry nil) (funcall entry 0)))
    ;; The last column takes the remaining width.
    (should (equal (nth 1 (aref format 1)) 0))
    (should (r4-columns-less-p "abc" "ABD"))
    (should (equal (r4-columns-valid '(slug nope tracks) specs) '(slug tracks)))))

(ert-deftest r4-test-channel-image-url ()
  (should (equal (r4-channel-image-url (r4-channel-create :image "abc") 100)
                 "https://res.cloudinary.com/radio4000/image/upload/w_100,h_100,c_thumb,q_80/abc.jpg"))
  (should-not (r4-channel-image-url (r4-channel-create :image "")))
  (should-not (r4-channel-image-url (r4-channel-create))))

(ert-deftest r4-test-files-sanitize ()
  ;; Like filenamify in the r4 command line, see features.org.
  (should (equal (r4-files-sanitize "AC/DC: \"Live\" <at> the *park*?") "AC!DC! !Live! !at! the !park!"))
  (should (equal (r4-files-sanitize "a//b::c") "a!b!c"))
  (should (equal (r4-files-sanitize "trailing dots...") "trailing dots"))
  (should (equal (r4-files-sanitize "con") "con!"))
  (should (equal (r4-files-sanitize "") "!"))
  (should (equal (r4-files-sanitize "zero​width") "zero!width"))
  (should (equal (length (r4-files-sanitize (make-string 300 ?é))) 180))
  ;; Used to hang, see `r4-download--sanitize' in older versions.
  (should (equal (r4-files-sanitize "Molam International Band - ลำสั้นดิสโก้")
                 "Molam International Band - ลำสั้นดิสโก้"))
  (should (equal (r4-files-sanitize "Shekarchi (شکارچی) - توله") "Shekarchi (شکارچی) - توله")))

(ert-deftest r4-test-files-basename ()
  (should (equal (r4-files-basename (r4-test--track :title "AC/DC" :provider "youtube" :media-id "xyz"))
                 "AC!DC [xyz]"))
  ;; Only YouTube ids are added, like the r4 command line.
  (should (equal (r4-files-basename (r4-test--track :title "Song" :provider "soundcloud" :media-id "123"))
                 "Song")))

(ert-deftest r4-test-download-command ()
  (let ((r4-download-directory "/music/")
        (r4-download-audio-format "flac")
        (job (r4-download-create :album "Radio: 100%"
                                 :track (r4-test--track :title "100% hits: live" :provider "youtube"
                                                        :media-id "m"))))
    (let ((command (r4-download--command job)))
      (should (member "/music/test/tracks/100%% hits! live [m].%(ext)s" command))
      (should (member "--extract-audio" command))
      (should (member "flac" command))
      (should (member "100%% hits\\: live:%(title)s" command))
      (should (member "Radio\\: 100%%:%(album)s" command))
      (should (equal (car (last command)) "https://www.youtube.com/watch?v=pmpXABDFMyE")))
    (setf (r4-download-kind job) 'video)
    (let ((command (r4-download--command job)))
      (should-not (member "--extract-audio" command))
      (should (member "--merge-output-format" command)))
    (let ((r4-download-metadata nil))
      (should-not (member "--parse-metadata" (r4-download--command job))))))

(ert-deftest r4-test-files-index ()
  (let* ((r4-download-directory (make-temp-file "r4-test" t))
         (track (r4-test--track :provider "youtube" :media-id "m"
                                :created-at "2020-01-02T03:04:05Z"))
         (directory (r4-files-tracks-directory "test")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (should-not (r4-files-track-file track))
          (write-region "" nil (expand-file-name "A track [m].webp" directory))
          (write-region "" nil (expand-file-name "A track [m].opus.part" directory))
          (should-not (r4-files-track-file track))
          (write-region "" nil (expand-file-name "A track [m].mkv" directory))
          (should (equal (file-name-nondirectory (r4-files-track-file track)) "A track [m].mkv"))
          (should-not (r4-files-track-file track 'audio))
          (write-region "" nil (expand-file-name "A track [m].opus" directory))
          ;; Audio is preferred, unless a kind is asked.
          (should (equal (file-name-nondirectory (r4-files-track-file track)) "A track [m].opus"))
          (should (equal (file-name-nondirectory (r4-files-track-file track 'video)) "A track [m].mkv"))
          (r4-files-write-playlist "test" (list track))
          (should (equal (with-temp-buffer
                           (insert-file-contents (expand-file-name "../test.m3u" directory))
                           (buffer-string))
                         "#EXTM3U\n#EXTINF:-1,A track\ntracks/A track [m].opus\n"))
          ;; The text file and dates of a download, like the r4 command line.
          (r4-files-finish-track track (r4-files-track-file track))
          (should (equal (with-temp-buffer
                           (insert-file-contents (expand-file-name "A track [m].txt" directory))
                           (buffer-string))
                         "A track\n\n  https://www.youtube.com/watch?v=pmpXABDFMyE"))
          (should (time-equal-p (file-attribute-modification-time
                                 (file-attributes (r4-files-track-file track)))
                                (date-to-time "2020-01-02T03:04:05Z"))))
      (delete-directory r4-download-directory t))))

(ert-deftest r4-test-files-migrate ()
  (let* ((r4-download-directory (make-temp-file "r4-test" t))
         (channel (expand-file-name "test" r4-download-directory)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "tracks" channel) t)
          (dolist (name '("Old [a].opus" "Old [a].png" "Old [a].webp" "Both [b].m4a" "notes.txt"))
            (write-region "" nil (expand-file-name name channel)))
          (write-region "" nil (expand-file-name "tracks/Both [b].m4a" channel))
          (cl-letf (((symbol-function 'yes-or-no-p) #'always))
            (r4-files-migrate))
          (should (equal (directory-files (expand-file-name "tracks" channel) nil "\\`[^.]")
                         '("Both [b].m4a" "Old [a].opus")))
          ;; Files already in tracks/ are left, and other files kept.
          (should (equal (directory-files channel nil "\\`[^.]")
                         '("Both [b].m4a" "notes.txt" "tracks"))))
      (delete-directory r4-download-directory t))))

(ert-deftest r4-test-files-channel ()
  (let* ((r4-download-directory (make-temp-file "r4-test" t))
         (channel (r4-channel-create :id "c1" :slug "test" :name "Test" :description "Hello"
                                     :image "img" :track-count 1))
         (track (r4-test--track)))
    (unwind-protect
        (progn
          (r4-files-write-channel channel (list track))
          (let ((text (with-temp-buffer
                        (insert-file-contents (expand-file-name "test/test.txt" r4-download-directory))
                        (buffer-string))))
            ;; radio4000.com imports the name, description, slug and id.
            (should (string-prefix-p "Test\n====\n\nHello\n\nInfo:\n  ID: c1\n  Slug: test\n" text)))
          (should (equal (with-temp-buffer
                           (insert-file-contents (expand-file-name "test/tracks.m3u" r4-download-directory))
                           (buffer-string))
                         "#EXTM3U\n#EXTINF:-1,A track\nhttps://www.youtube.com/watch?v=pmpXABDFMyE\n"))
          (should (string-match-p "/image/upload/.*/img\\.jpg\n"
                                  (with-temp-buffer
                                    (insert-file-contents (expand-file-name "test/image.url" r4-download-directory))
                                    (buffer-string))))
          ;; Failed downloads are remembered, until retried.
          (r4-files-add-failure track "ERROR: gone")
          (should (equal (r4-files-failed-ids "test") (list (r4-track-id track))))
          (r4-files-forget-failures "test")
          (should-not (r4-files-failed-ids "test")))
      (delete-directory r4-download-directory t))))

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
          ;; Start a track at a position, once loaded.
          (r4-player-play tracks 0 0.3 "test")
          (should (equal r4-player-label "test"))
          (with-timeout (5 (ert-fail "No seek"))
            (while r4-player--start-at (accept-process-output nil 0.05)))
          (r4-player-toggle-video)
          (r4-player-toggle-video)
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
    (r4-player-enqueue (list one two))
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

(ert-deftest r4-test-history ()
  (let* ((r4-history-file (make-temp-file "r4-history"))
         (r4-history--entries nil) (r4-history--loaded t)
         (one (r4-test--track :id "1" :title "one"))
         (r4-player-queue (vector one)) (r4-player-index 0))
    (unwind-protect
        (cl-letf (((symbol-function 'r4-player-live-p) #'always))
          (r4-history--record)
          (r4-history--record)
          (should (equal (length r4-history--entries) 1))
          ;; It is saved, and loaded back.
          (setq r4-history--entries nil r4-history--loaded nil)
          (r4-history--load)
          (should (equal (r4-track-title (cdar r4-history--entries)) "one"))
          (r4-history)
          (with-current-buffer "*r4 history*"
            (should (equal (car (aref tabulated-list-format 1)) "Played"))
            (should (equal (r4-track-title (tabulated-list-get-id (point-min))) "one")))
          (kill-buffer "*r4 history*"))
      (delete-file r4-history-file))))

(ert-deftest r4-test-menus ()
  (dolist (menu '(r4 r4-tracks-menu r4-channels-menu r4-queue-menu))
    (should (commandp menu)))
  (should (eq (keymap-lookup r4-tracks-mode-map "?") 'r4-tracks-menu))
  ;; The player keys work in every list.
  (dolist (map (list r4-tracks-mode-map r4-channels-mode-map r4-queue-mode-map))
    (should (eq (keymap-lookup map ">") 'r4-player-next))
    (should (eq (keymap-lookup map "S") 'tabulated-list-sort))
    (should (eq (keymap-lookup map "n") 'next-line))))

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

(ert-deftest r4-test-tracks-status-update ()
  (let ((buffer (generate-new-buffer "r4-test"))
        (r4-download-directory (make-temp-file "r4-test" t))
        (one (r4-test--track :id "11111111-a" :title "one"))
        (two (r4-test--track :id "22222222-b" :title "two")))
    (unwind-protect
        (with-current-buffer buffer
          (r4-tracks-mode)
          (r4-tracks--set buffer (list one two))
          (let ((r4-tracks--playing-id "22222222-b"))
            (r4-tracks-update-status '("22222222-b")))
          (goto-char (point-min))
          (should (equal (aref (tabulated-list-get-entry) 0) ""))
          (forward-line 1)
          (should (equal (aref (tabulated-list-get-entry) 0) "▶"))
          (should (looking-at "  ▶")))
      (kill-buffer buffer)
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

(defun r4-test--row-match-p (row params)
  "Return non-nil if ROW matches the eq. and in. filters of PARAMS."
  (cl-every (lambda (param)
              (let ((value (alist-get (intern (car param)) row))
                    (filter (cadr param)))
                (if (string-match "\\`in\\.(\\(.*\\))\\'" filter)
                    (member value (split-string (match-string 1 filter) ","))
                  (equal value (string-remove-prefix "eq." filter)))))
            params))

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
     ;; Reads are public, like on radio4000.com.
     (unless (member token '(nil "token-1")) (signal 'r4-api-error '("unauthorized")))
     (let ((rows (alist-get (intern table) r4-test--db)))
       (dolist (column '("id" "slug" "channel_id" "user_id" "follower_id"))
         (when-let* ((value (r4-test--param params column)))
           (setq rows (cl-remove-if-not
                       (lambda (row) (equal (alist-get (intern column) row) value))
                       rows))))
       rows))
    (`("DELETE" ,(rx "/rest/v1/" (let table (+ anychar))))
     (unless (equal token "token-1") (signal 'r4-api-error '("unauthorized")))
     (let ((key (intern table)))
       (setf (alist-get key r4-test--db)
             (cl-remove-if (lambda (row) (r4-test--row-match-p row params))
                           (alist-get key r4-test--db))))
     nil)
    (`("PATCH" ,(rx "/rest/v1/" (let table (+ anychar))))
     (unless (equal token "token-1") (signal 'r4-api-error '("unauthorized")))
     (dolist (row (alist-get (intern table) r4-test--db))
       (when (r4-test--row-match-p row params)
         (pcase-dolist (`(,field . ,value) body)
           (setf (alist-get field row) value))))
     nil)
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

(ert-deftest r4-test-follows ()
  (r4-test--with-backend
    (push '((id . "mine") (slug . "me") (name . "Me")) (alist-get 'channels r4-test--db))
    (push '((user_id . "user-1") (channel_id . "mine")) (alist-get 'user_channel r4-test--db))
    (cl-letf (((symbol-function 'r4-sync-my-channels)
               (lambda () (list (r4-channel-create :id "mine" :slug "me"))))
              ((symbol-function 'y-or-n-p) #'always))
      (let ((r4-follows--my-channel nil)
            (other (r4-channel-create :id "other" :slug "other"))
            (buffer (generate-new-buffer "r4-test")))
        (unwind-protect
            (with-current-buffer buffer
              (r4-channels-mode)
              (setq tabulated-list-entries (r4-channels--entries (list other)))
              (tabulated-list-print)
              (goto-char (point-min))
              (r4-channels-follow)
              (should (equal (alist-get 'followers r4-test--db)
                             '(((follower_id . "mine") (channel_id . "other")))))
              (should (equal (r4-follows-following-ids "mine") '("other")))
              ;; Following again unfollows.
              (r4-channels-follow)
              (should (equal (car r4-test--requests)
                             '("DELETE" "/rest/v1/followers"
                               (("follower_id" "eq.mine") ("channel_id" "eq.other"))
                               nil "token-1"))))
          (kill-buffer buffer))))))

(defmacro r4-test--with-answers (answers &rest body)
  "Run BODY, answering the minibuffer prompts with ANSWERS, in order."
  (declare (indent 1))
  `(let ((answers ,answers))
     (cl-letf (((symbol-function 'read-string)
                (lambda (&rest _) (or (pop answers) (error "No more answers")))))
       ,@body)))

(ert-deftest r4-test-edit ()
  (r4-test--with-backend
    (push '((id . "mine") (slug . "me") (name . "Me")) (alist-get 'channels r4-test--db))
    (cl-letf (((symbol-function 'r4-sync-my-channels)
               (lambda () (list (r4-channel-create :id "mine" :slug "me" :name "Me")))))
      (let ((r4-edit--my-channels nil)
            (buffer (generate-new-buffer "r4-test")))
        (unwind-protect
            (progn
              ;; Add a track, with its title from oEmbed.
              (cl-letf (((symbol-function 'r4-edit-media-title) (lambda (_) "From oEmbed")))
                (r4-test--with-answers (list "https://youtu.be/aaaaaaaaaaa" "Song" "#tag")
                  (r4-add-track "me")))
              (let ((track (car (alist-get 'tracks r4-test--db))))
                (should (equal (alist-get 'title track) "Song"))
                (should (equal (alist-get 'channel_id (car (alist-get 'channel_track r4-test--db)))
                               "mine"))
                ;; Edit it in a list of the channel.
                (with-current-buffer buffer
                  (r4-tracks-mode)
                  (setq r4-tracks--source '(:slug "me"))
                  (r4-tracks--set buffer (list (r4-test--track :id (alist-get 'id track)
                                                               :slug "me" :title "Song")))
                  (goto-char (point-min))
                  (cl-letf (((symbol-function 'revert-buffer) #'ignore))
                    (r4-test--with-answers (list "https://youtu.be/bbbbbbbbbbb" "New title" "")
                      (r4-tracks-edit))
                    (should (equal (alist-get 'title (car (alist-get 'tracks r4-test--db)))
                                   "New title"))
                    (r4-tracks-delete)
                    (should-not (alist-get 'tracks r4-test--db))))))
          (kill-buffer buffer))
        ;; Channels.
        (r4-test--with-answers (list "Me Too" "me" "Hello" "")
          (r4-edit-channel "me"))
        (should (equal (alist-get 'name (car (alist-get 'channels r4-test--db))) "Me Too"))
        (should-error (r4-create-channel "Taken" "me") :type 'user-error)
        (r4-create-channel "New" "new")
        (should (alist-get 'user_channel r4-test--db))
        (r4-test--with-answers (list "nope")
          (should-error (r4-delete-channel "me") :type 'user-error))
        (r4-test--with-answers (list "me")
          (r4-delete-channel "me"))
        (should-not (cl-find "mine" (alist-get 'channels r4-test--db)
                             :key (lambda (row) (alist-get 'id row)) :test #'equal))))))

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
