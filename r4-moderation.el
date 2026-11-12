;;; r4-moderation.el --- Quarantine spam accounts, for admins  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Admins of radio4000.com quarantine the accounts that fill channels
;; with spam.  Quarantine hides all the channels of an account from
;; everyone but admins, and keeps it from signing in; the database
;; deletes it after 30 days, unless an admin releases it.  See
;; docs/moderation.md of radio4000/supabase.
;;
;; K quarantines the account of the channel at point, in channel lists,
;; or releases it if quarantined.  `r4-quarantine' lists the
;; quarantined accounts, with the day each will be deleted; K releases
;; the one at point.  Only admins can: accounts with "admin" in their
;; app_metadata, which the database owners set.

;;; Code:

(require 'seq)
(require 'tabulated-list)
(require 'r4-models)
(require 'r4-api)
(require 'r4-auth)
(require 'r4-player)
(require 'r4-channels)

(defconst r4-quarantine-buffer "*r4 quarantine*"
  "Name of the buffer listing the quarantined accounts.")

(defun r4-moderation--require-admin ()
  "Signal an error unless the signed in user is an admin."
  (r4-auth-require)
  (unless (r4-auth-admin-p)
    (user-error "Only radio4000.com admins quarantine accounts")))

(defun r4-moderation--rpc (function &optional args)
  "Call the database FUNCTION with ARGS, an alist, as the signed in user."
  (r4-api-request "POST" (concat "/rest/v1/rpc/" function)
                  :body (or args (make-hash-table))
                  :token (r4-auth-token)))

(defun r4-quarantine-accounts ()
  "Return the quarantined accounts, the oldest first.
Each is an alist with user_id, channel_slugs, quarantined_at,
quarantined_by and purge_at."
  (r4-moderation--require-admin)
  (r4-moderation--rpc "quarantined_accounts"))

(defun r4-quarantine--account (slug &optional accounts)
  "Return the quarantined account of the channel SLUG, or nil.
Look in ACCOUNTS, else ask the database."
  (seq-find (lambda (account) (member slug (alist-get 'channel_slugs account)))
            (or accounts (r4-quarantine-accounts))))

(defun r4-quarantine--day (timestamp)
  "Return the day of TIMESTAMP, a string from the database."
  (if timestamp (format-time-string "%F" (date-to-time timestamp)) ""))

(defun r4-quarantine-toggle (slug)
  "Quarantine the account of the channel SLUG, or release it."
  (r4-moderation--require-admin)
  (if-let* ((account (r4-quarantine--account slug)))
      (let ((slugs (mapconcat (lambda (s) (concat "@" s)) (alist-get 'channel_slugs account) " ")))
        (when (yes-or-no-p (format "Release the account of %s? " slugs))
          (r4-moderation--rpc "release_user_by_channel_slug" `((channel_slug . ,slug)))
          (message "r4: released the account of %s" slugs)))
    (when (yes-or-no-p
           (format "Hide all the channels of @%s's account, block its sign in, and delete it in 30 days unless released.  Quarantine it? "
                   slug))
      (r4-moderation--rpc "quarantine_user_by_channel_slug" `((channel_slug . ,slug)))
      (message "r4: quarantined the account of @%s, deleted on %s unless released"
               slug (r4-quarantine--day
                     (alist-get 'purge_at (r4-quarantine--account slug))))))
  (when-let* ((buffer (get-buffer r4-quarantine-buffer)))
    (with-current-buffer buffer (revert-buffer))))

(defun r4-channels-quarantine ()
  "Quarantine the account of the channel at point, or release it."
  (interactive)
  (r4-quarantine-toggle (r4-channel-slug (r4-channels--at-point))))

;;;; List of quarantined accounts

(defun r4-quarantine--entries (accounts)
  "Return the `tabulated-list-entries' of the quarantined ACCOUNTS."
  (let ((me (plist-get (r4-auth-session) :user-id)))
    (mapcar (lambda (account)
              (let-alist account
                (list account
                      (vector (mapconcat (lambda (s) (concat "@" s)) .channel_slugs " ")
                              (r4-quarantine--day .quarantined_at)
                              (r4-quarantine--day .purge_at)
                              (cond ((null .quarantined_by) "")
                                    ((equal .quarantined_by me) "you")
                                    (t (substring .quarantined_by 0 8)))))))
            accounts)))

(defun r4-quarantine-revert (&rest _)
  "Fetch the quarantined accounts again."
  (let ((accounts (r4-quarantine-accounts)))
    (setq tabulated-list-entries (r4-quarantine--entries accounts))
    (tabulated-list-print t)
    (message "%d quarantined account%s" (length accounts) (if (cdr accounts) "s" ""))))

(defun r4-quarantine--slug-at-point ()
  "Return the first channel slug of the account at point."
  (car (alist-get 'channel_slugs (or (tabulated-list-get-id)
                                     (user-error "No account at point")))))

(defun r4-quarantine-release ()
  "Release the account at point."
  (interactive)
  (r4-quarantine-toggle (r4-quarantine--slug-at-point)))

(defun r4-quarantine-visit ()
  "Visit the first channel of the account at point."
  (interactive)
  (r4-channel-page (r4-quarantine--slug-at-point)))

(declare-function r4-channel-page "r4-channel-page")

(defvar-keymap r4-quarantine-mode-map
  :doc "Keymap for `r4-quarantine-mode'."
  :parent (r4-player-list-map)
  "RET" #'r4-quarantine-visit
  "K" #'r4-quarantine-release)

(define-derived-mode r4-quarantine-mode tabulated-list-mode "r4 quarantine"
  "Major mode listing the quarantined radio4000.com accounts.

\\{r4-quarantine-mode-map}"
  (setq tabulated-list-format [("channel slugs" 40 t) ("quarantined at" 14 t)
                               ("purge at" 14 t) ("quarantined by" 14 t)]
        tabulated-list-padding 2)
  (setq-local revert-buffer-function #'r4-quarantine-revert)
  (tabulated-list-init-header))

;;;###autoload (autoload 'r4-quarantine "r4" nil t)
(defun r4-quarantine ()
  "List the quarantined accounts, for admins.  K releases one."
  (interactive)
  (r4-moderation--require-admin)
  (with-current-buffer (get-buffer-create r4-quarantine-buffer)
    (r4-quarantine-mode)
    (revert-buffer)
    (pop-to-buffer-same-window (current-buffer))))

(keymap-set r4-channels-mode-map "K" #'r4-channels-quarantine)

(provide 'r4-moderation)

;;; r4-moderation.el ends here
