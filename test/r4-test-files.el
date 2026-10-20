;;; r4-test-files.el --- Tests of r4: Files and downloads  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'r4-test-helpers)

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

(ert-deftest r4-test-synced-channels ()
  (let ((r4-synced-channels nil) (r4-synced--last-check nil) (r4-sync-interval 3600)
        (r4-synced-save-function (lambda (symbol value) (set symbol value)))
        (r4-download-program "true")
        downloaded)
    (cl-letf (((symbol-function 'r4-api-channel)
               (lambda (slug callback) (funcall callback (r4-channel-create :slug slug :name "C"))))
              ((symbol-function 'r4-api-tracks)
               (lambda (callback &rest _) (funcall callback (list (r4-test--track)))))
              ((symbol-function 'r4-download-tracks)
               (lambda (tracks &rest args) (push (cons (length tracks) args) downloaded))))
      ;; Syncing a channel downloads its new tracks at once.
      (r4-toggle-synced-channel "oskar")
      (should (equal r4-synced-channels '("oskar")))
      (should (equal (length downloaded) 1))
      (should (plist-get (cdar downloaded) :channel))
      ;; Opening a list checks again only after r4-sync-interval.
      (r4-sync-channels-maybe)
      (should (equal (length downloaded) 1))
      (setq r4-synced--last-check (- (float-time) 3601))
      (r4-sync-channels-maybe)
      (should (equal (length downloaded) 2))
      ;; The tracks of synced channels download while playing.
      (setq downloaded nil)
      (let ((r4-player-queue (vector (r4-test--track :slug "oskar")))
            (r4-player-index 0)
            (r4-download-directory (make-temp-file "r4-test" t)))
        (cl-letf (((symbol-function 'r4-player-live-p) #'always))
          (r4-synced--download-playing)
          (should (equal (length downloaded) 1))
          (setq downloaded nil)
          (let ((r4-player-download nil)) (r4-synced--download-playing))
          (should-not downloaded)
          (aset r4-player-queue 0 (r4-test--track :slug "other"))
          (r4-synced--download-playing)
          (should-not downloaded))
        (delete-directory r4-download-directory t))
      (r4-toggle-synced-channel "oskar")
      (should-not r4-synced-channels))))

(provide 'r4-test-files)

;;; r4-test-files.el ends here
