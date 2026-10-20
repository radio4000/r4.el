;;; r4-test-account.el --- Tests of r4: Sign in, sync, follows and editing  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'r4-test-helpers)

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
              (cl-letf (((symbol-function 'r4-api-media-title) (lambda (_) "From oEmbed")))
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

(provide 'r4-test-account)

;;; r4-test-account.el ends here
