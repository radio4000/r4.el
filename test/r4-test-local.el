;;; r4-test-local.el --- Tests of r4: Local channels  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'r4-test-helpers)

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

(provide 'r4-test-local)

;;; r4-test-local.el ends here
