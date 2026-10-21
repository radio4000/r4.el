;;; r4-test-lists.el --- Tests of r4: Track lists  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'r4-test-helpers)

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
          (should (equal (substring-no-properties mode-line-process) " #jazz 2/3"))
          (r4-tracks-filter "#jazz miles")
          (should (equal (mapcar (lambda (e) (r4-track-id (car e))) tabulated-list-entries) '("1")))
          (r4-tracks-filter "@else")
          (should (equal (mapcar (lambda (e) (r4-track-id (car e))) tabulated-list-entries) '("3")))
          (r4-tracks-filter "")
          (should (equal (length tabulated-list-entries) 3))
          (should-not mode-line-process)
          (should (equal (r4-filter-tag-counts tracks) '(("jazz" . 2) ("dub" . 1))))
          ;; # toggles tags, offering those of the filter first, to remove them.
          (r4-tracks-filter "miles")
          (let (offered)
            (cl-letf (((symbol-function 'completing-read-multiple)
                       (lambda (_prompt table &rest _)
                         (setq offered (funcall table "" nil t))
                         '("jazz"))))
              (call-interactively #'r4-tracks-filter-tag)
              (should (equal r4-tracks--filter-text "miles #jazz"))
              (call-interactively #'r4-tracks-filter-tag)
              (should (equal offered '("jazz")))
              (should (equal r4-tracks--filter-text "miles"))))
          ;; Clicking a tag of a track toggles it, and in the mode line.
          (r4-tracks-filter "")
          (goto-char (point-min))
          (search-forward "#dub")
          (let ((click `(mouse-1 (,(selected-window) ,(1- (point)) (0 . 0) 0))))
            (should (eq (get-text-property (1- (point)) 'keymap) r4-filter-tag-map))
            (set-window-buffer (selected-window) buffer)
            (r4-filter-toggle-tag-at-mouse click)
            (should (equal r4-tracks--filter-text "#dub"))
            (should (equal (length tabulated-list-entries) 1))
            (let ((chip (string-search "#dub" mode-line-process)))
              (should (eq (get-text-property chip 'local-map mode-line-process) r4-filter-tag-map))
              (r4-filter-toggle-tag-at-mouse
               `(mouse-1 (,(selected-window) mode-line (0 . 0) 0 (,mode-line-process . ,chip))))
              (should-not r4-tracks--filter-text))))
      (kill-buffer buffer))))

(ert-deftest r4-test-filter-toggle-tags ()
  (should (equal (r4-filter-toggle-tags nil '("Jazz")) "#jazz"))
  (should (equal (r4-filter-toggle-tags "@x #jazz miles" '("jazz" "dub")) "@x miles #dub"))
  (should-not (r4-filter-toggle-tags "#jazz" '("jazz"))))

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
          (should (equal (aref (tabulated-list-get-entry) 0) "  "))
          (forward-line 1)
          (should (equal (aref (tabulated-list-get-entry) 0) " ▶"))
          (should (looking-at "   ▶")))
      (kill-buffer buffer)
      (delete-directory r4-download-directory t))))

(ert-deftest r4-test-tracks-status ()
  (let* ((r4-download-directory (make-temp-file "r4-test" t))
         (track (r4-test--track :id "11111111-a" :title "one" :provider "youtube" :media-id "m"))
         (r4-tracks--playing-id nil)
         (played nil)
         (r4-tracks-played-function (lambda (_) played)))
    (unwind-protect
        (progn
          (should (equal (r4-tracks--status track) "  "))
          (r4-files-add-failure track "ERROR")
          (should (equal (r4-tracks--status track) "✗ "))
          (make-directory (r4-files-tracks-directory "test") t)
          (write-region "" nil (expand-file-name "one [m].opus" (r4-files-tracks-directory "test")))
          (should (equal (r4-tracks--status track) "✓ "))
          (setq r4-tracks--playing-id "11111111-a")
          (should (equal (r4-tracks--status track) "✓▶"))
          (setq r4-tracks--playing-id nil played t)
          (should (equal (r4-tracks--status track) "✓·")))
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

(ert-deftest r4-test-channels-filter ()
  (let ((buffer (generate-new-buffer "r4-test")))
    (unwind-protect
        (with-current-buffer buffer
          (r4-channels-mode)
          (setq r4-channels--channels
                (list (r4-channel-create :slug "oskar" :name "Radio Oskar" :description "perfume")
                      (r4-channel-create :slug "ko002" :name "Ko" :description "house")))
          (r4-channels-redisplay)
          (r4-channels-filter "radio perfume")
          (should (equal (mapcar (lambda (e) (r4-channel-slug (car e))) tabulated-list-entries)
                         '("oskar")))
          (should (equal mode-line-process " 1/2"))
          (r4-channels-filter "")
          (should (equal (length tabulated-list-entries) 2)))
      (kill-buffer buffer))))

(ert-deftest r4-test-explore-featured-score ()
  ;; Like featuredScore of radio4000.com: followers, then tracks, then activity.
  (let ((now (format-time-string "%FT%TZ" nil t)))
    (should (> (r4-explore-featured-score
                (r4-channel-create :followers '("a" "b" "c" "d" "e") :track-count 10
                                   :latest-track-at now))
               (r4-explore-featured-score
                (r4-channel-create :followers nil :track-count 100 :latest-track-at now))))
    (should (< (abs (- (r4-explore-featured-score
                        (r4-channel-create :followers nil :track-count 9 :latest-track-at now))
                       (+ (* 2 (log 10)) 3)))
               0.0001))))

(ert-deftest r4-test-describe-track ()
  (let ((r4-download-directory (make-temp-file "r4-test" t)))
    (unwind-protect
        (progn
          (r4-describe-track (r4-test--track :title "Song" :tags '("jazz") :duration 61
                                             :description "via @oskar"))
          (with-current-buffer "*r4 track Song*"
            (let ((text (buffer-string)))
              (should (string-match-p "Tags: +#jazz" text))
              (should (string-match-p "Duration: +1:01" text))
              (should (string-match-p "\\[Play next\\]" text))))
          (kill-buffer "*r4 track Song*"))
      (delete-directory r4-download-directory t))))

;; Like /@slug on radio4000.com, with links, tags and the latest tracks.
(ert-deftest r4-test-channel-page ()
  (let* ((channel (r4-channel-create :id "c1" :slug "ko002" :name "Ko"
                                     :description "Dub and #jazz, with @oskar https://ko.org"
                                     :track-count 7 :latitude 52.5 :longitude 13.4))
         (tracks (cl-loop for i from 1 to 7
                          collect (r4-test--track :id (format "%d" i) :slug "ko002"
                                                  :title (format "Song %d" i)
                                                  :tags (if (cl-oddp i) '("jazz") '("dub")))))
         (r4-channel-image-size 0)
         visited filtered played)
    (cl-letf (((symbol-function 'r4-api-tracks)
               (lambda (callback &rest _) (funcall callback tracks)))
              ((symbol-function 'r4-player-play)
               (lambda (queue &optional index &rest _) (setq played (cons (length queue) index))))
              ((symbol-function 'r4-tracks-buffer)
               (lambda (source) (setq visited source) (get-buffer-create "*r4 test tracks*")))
              ((symbol-function 'r4-tracks-set-filter)
               (lambda (text) (setq filtered text))))
      (r4-channel-page channel)
      (unwind-protect
          (with-current-buffer "*r4 @ko002 channel*"
            (should (eq major-mode 'r4-channel-page-mode))
            (should (string-match-p "\\`Ko  @ko002\n" (buffer-string)))
            (dolist (text '("52.50, 13.40" "▶ Play" "Tracks 7" "#jazz 4" "#dub 3"
                            "Song 5" "See all 7 tracks"))
              (should (string-search text (buffer-string))))
            ;; 5 latest tracks.
            (should-not (string-search "Song 6" (buffer-string)))
            ;; The tags of the description open the tracks with that tag.
            (goto-char (point-min))
            (search-forward "#jazz,")
            (push-button (- (point) 3))
            (should (equal visited '(:slug "ko002")))
            (should (equal filtered "#jazz"))
            (set-buffer "*r4 @ko002 channel*")
            ;; Mentions are buttons too.
            (should (button-at (1+ (string-search "@oskar" (buffer-string)))))
            ;; A track plays the channel from it.
            (search-forward "Song 2")
            (push-button (1- (point)))
            (should (equal played '(7 . 1)))
            (should (eq (keymap-lookup r4-channel-page-mode-map "t") 'r4-channel-page-tracks)))
        (kill-buffer "*r4 @ko002 channel*")
        (kill-buffer "*r4 test tracks*")))
    ;; RET in channel lists visits the page, T the tracks.
    (should (eq (keymap-lookup r4-channels-mode-map "RET") 'r4-channels-visit))
    (should (eq (keymap-lookup r4-channels-mode-map "T") 'r4-channels-open))))

(ert-deftest r4-test-bookmark-tracks ()
  (let ((r4-history-file nil) (r4-history--entries nil) (r4-history--loaded t))
    (with-current-buffer (r4-tracks-buffer '(:history t :name "history"))
      (r4-tracks-filter "#jazz")
      (let ((record (r4-bookmark-make-tracks-record)))
        (should (equal (car record) "r4 history"))
        (kill-buffer)
        (r4-bookmark-jump-tracks record)
        (should (equal r4-tracks--source '(:history t :name "history")))
        (should (equal r4-tracks--filter-text "#jazz"))
        (should r4-tracks--filter)
        (kill-buffer)))))

(ert-deftest r4-test-bookmark-channels ()
  (let (called)
    (cl-letf (((symbol-function 'r4-following)
               (lambda (slug) (setq called slug) (set-buffer (get-buffer-create "r4-test-following")))))
      (r4-bookmark-jump-channels '("x" (r4-command r4-following "oskar")))
      (should (equal called "oskar"))
      (should (equal (buffer-name) "r4-test-following"))
      (kill-buffer)
      ;; Only r4 commands run.
      (should-error (r4-bookmark-jump-channels '("x" (r4-command delete-file "/tmp/x")))
                    :type 'user-error))))

(ert-deftest r4-test-list-fetch-failed ()
  (let ((r4-tracks-fetch-functions
         (list (lambda (_source _callback) (error "Network down")))))
    (with-current-buffer (r4-tracks-buffer '(:test t :name "failing"))
      (should (string-match-p "failed" mode-line-process))
      (kill-buffer)))
  ;; Errors of asynchronous requests reach the function bound at the call.
  (let (failure)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (with-temp-buffer
                   (setq-local url-http-end-of-headers (point-min))
                   (funcall callback '(:error (error http 500)))))))
      (let ((r4-api-error-function (lambda (error) (setq failure error))))
        (r4-api-get "channels" nil #'ignore))
      (should (string-match-p "http 500" failure)))))

(ert-deftest r4-test-channels-cache ()
  (let ((r4-channels-cache-file (make-temp-file "r4-channels"))
        (r4-channels--cache nil))
    (unwind-protect
        (cl-letf (((symbol-function 'r4-api-channels) #'ignore))
          (r4-channels--set-cache (list (r4-channel-create :slug "oskar")))
          (setq r4-channels--cache nil)
          (should (equal (mapcar #'r4-channel-slug (r4-channels--all)) '("oskar"))))
      (delete-file r4-channels-cache-file))))

(provide 'r4-test-lists)

;;; r4-test-lists.el ends here
