;;; r4-auth.el --- Sign in to Radio4000  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Sign in to a radio4000.com account through Supabase Auth, with a
;; code sent by email (like the web app), or with a password read from
;; `auth-source'.  The session is refreshed automatically and saved in
;; `r4-auth-session-file', readable only by the user.
;;
;; Admins also verify codes from an authenticator app (2FA) to
;; moderate: `r4-auth-setup-code' once, then `r4-auth-verify-code'.

;;; Code:

(require 'auth-source)
(require 'seq)
(require 'r4-api)

(defcustom r4-auth-source-host "radio4000.com"
  "Host of the `auth-source' entry holding the radio4000.com password.
For example, in ~/.authinfo.gpg:
  machine radio4000.com login you@example.org password secret"
  :type 'string
  :group 'r4)

(defcustom r4-auth-session-file (locate-user-emacs-file "r4-session.eld")
  "File where the session is saved between Emacs sessions.
nil keeps the session in memory only."
  :type '(choice (file :tag "File") (const :tag "Do not save" nil))
  :group 'r4)

(defvar r4-auth--session nil
  "Plist of the current session, or nil when signed out.
Its keys are :access-token, :refresh-token, :expires-at, :user-id,
:email and :admin.")

;;;; Session

(defun r4-auth--load ()
  "Load the saved session, if any."
  (when (and (not r4-auth--session) r4-auth-session-file
             (file-readable-p r4-auth-session-file))
    (setq r4-auth--session
          (ignore-errors
            (with-temp-buffer
              (insert-file-contents r4-auth-session-file)
              (read (current-buffer)))))))

(defun r4-auth--save ()
  "Save the current session, or delete the saved one when signed out."
  (when r4-auth-session-file
    (if r4-auth--session
        (progn
          (make-directory (file-name-directory (expand-file-name r4-auth-session-file)) t)
          (with-file-modes #o600
            (with-temp-file r4-auth-session-file
              (prin1 r4-auth--session (current-buffer)))))
      (when (file-exists-p r4-auth-session-file)
        (delete-file r4-auth-session-file)))))

(defun r4-auth--set-session (response)
  "Set the session from the Supabase Auth RESPONSE and return it."
  (let-alist response
    (setq r4-auth--session
          (list :access-token .access_token
                :refresh-token .refresh_token
                :expires-at (or .expires_at (+ (floor (float-time)) .expires_in))
                :user-id .user.id
                :email .user.email
                ;; Set by the radio4000.com database owners only.
                :admin (eq .user.app_metadata.admin t))))
  (r4-auth--save)
  r4-auth--session)

(defun r4-auth--request (path body)
  "POST BODY to the Supabase Auth endpoint PATH and return the response."
  (r4-api-request "POST" (concat "/auth/v1/" path) :body body))

(defun r4-auth--refresh ()
  "Refresh the session with its refresh token."
  (condition-case err
      (r4-auth--set-session
       (r4-auth--request "token?grant_type=refresh_token"
                         `((refresh_token . ,(plist-get r4-auth--session :refresh-token)))))
    (r4-api-error
     (setq r4-auth--session nil)
     (r4-auth--save)
     (user-error "Your session expired (%s), please sign in with `r4-login'"
                 (error-message-string err)))))

(defun r4-auth-session ()
  "Return the current session, refreshed if needed, or nil if signed out."
  (r4-auth--load)
  (when r4-auth--session
    (when (< (- (plist-get r4-auth--session :expires-at) (float-time)) 60)
      (r4-auth--refresh))
    r4-auth--session))

(defun r4-auth-signed-in-p ()
  "Return non-nil if signed in, without checking the session online."
  (r4-auth--load)
  (and r4-auth--session t))

(defun r4-auth-require ()
  "Return the current session, signing in first if needed."
  (or (r4-auth-session)
      (progn (call-interactively #'r4-login)
             (or (r4-auth-session) (user-error "Not signed in")))))

(defun r4-auth-admin-p ()
  "Return non-nil if the signed in user is a radio4000.com admin.
Admins moderate, see `r4-quarantine'.  Read from the saved session,
without checking it online: the database checks it again."
  (r4-auth--load)
  (plist-get r4-auth--session :admin))

(defun r4-auth-token ()
  "Return a valid access token, signing in first if needed."
  (plist-get (r4-auth-require) :access-token))

(defun r4-auth-user-id ()
  "Return the id of the signed in user, signing in first if needed."
  (plist-get (r4-auth-require) :user-id))

;;;; Commands

(defun r4-auth--password (email)
  "Return the password of EMAIL from `auth-source', or nil."
  (when-let* ((entry (car (auth-source-search :host r4-auth-source-host
                                              :user email :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun r4-login-with-password (email &optional password)
  "Sign in as EMAIL with PASSWORD.
Without PASSWORD, read it from `auth-source' (see
`r4-auth-source-host'), or else from the minibuffer."
  (interactive (list (read-string "Email: " (plist-get r4-auth--session :email))))
  (let ((password (or password (r4-auth--password email)
                      (read-passwd (format "Password for %s: " email)))))
    (r4-auth--set-session
     (r4-auth--request "token?grant_type=password"
                       `((email . ,email) (password . ,password))))
    (message "Signed in to Radio4000 as %s" email)))

(defun r4-login-with-email-code (email)
  "Sign in as EMAIL with a one-time code sent by email."
  (interactive (list (read-string "Email: " (plist-get r4-auth--session :email))))
  (r4-auth--request "otp" `((email . ,email) (create_user . :false)))
  (let ((code (string-trim (read-string (format "Code sent to %s: " email)))))
    (r4-auth--set-session
     (r4-auth--request "verify" `((type . "email") (email . ,email) (token . ,code))))
    (message "Signed in to Radio4000 as %s" email)))

;;;###autoload (autoload 'r4-login "r4" nil t)
(defun r4-login (email)
  "Sign in to radio4000.com as EMAIL.
Use the password from `auth-source' if there is one, or else ask
for a code sent by email or for the password."
  (interactive (list (read-string "Email: " (plist-get r4-auth--session :email))))
  (if-let* ((password (r4-auth--password email)))
      (r4-login-with-password email password)
    (pcase (read-answer "Sign in with: "
                        '(("code" ?c "a one-time code sent by email")
                          ("password" ?p "your password")))
      ("code" (r4-login-with-email-code email))
      (_ (r4-login-with-password email)))))

;;;###autoload (autoload 'r4-logout "r4" nil t)
(defun r4-logout ()
  "Sign out of radio4000.com."
  (interactive)
  (when-let* ((session (ignore-errors (r4-auth-session))))
    (ignore-errors
      (r4-api-request "POST" "/auth/v1/logout"
                      :token (plist-get session :access-token))))
  (setq r4-auth--session nil)
  (r4-auth--save)
  (message "Signed out of Radio4000"))

;;;; Codes from an authenticator app (2FA)

;; radio4000.com admins need a code verified in the last minutes to
;; moderate, so that their everyday session can't.  Other accounts
;; never set one up.

(defun r4-auth--factors ()
  "Return the authenticator apps (TOTP factors) of the signed in account."
  (seq-filter (lambda (factor) (equal (alist-get 'factor_type factor) "totp"))
              (alist-get 'factors (r4-api-request "GET" "/auth/v1/user"
                                                  :token (r4-auth-token)))))

(defun r4-auth--verify-factor (id)
  "Ask for a code of the authenticator app ID, verify it, and update the session."
  (let* ((token (r4-auth-token))
         (challenge (r4-api-request "POST" (format "/auth/v1/factors/%s/challenge" id)
                                    :body (make-hash-table) :token token))
         (code (string-trim (read-string "Code from your authenticator app: "))))
    (r4-auth--set-session
     (r4-api-request "POST" (format "/auth/v1/factors/%s/verify" id)
                     :body `((challenge_id . ,(alist-get 'id challenge)) (code . ,code))
                     :token token))))

;;;###autoload (autoload 'r4-auth-verify-code "r4" nil t)
(defun r4-auth-verify-code ()
  "Ask for a code from your authenticator app, and verify it.
For a few minutes, the session can then do what needs a code,
such as quarantine (see `r4-quarantine')."
  (interactive)
  (r4-auth-require)
  (let ((factor (seq-find (lambda (factor) (equal (alist-get 'status factor) "verified"))
                          (r4-auth--factors))))
    (unless factor
      (user-error "No authenticator app on this account, set one up with `r4-auth-setup-code'"))
    (r4-auth--verify-factor (alist-get 'id factor))
    (message "r4: code verified")))

(defun r4-auth--show-qr-code (factor)
  "Show the QR code and the secret of the new authenticator app FACTOR."
  (let-alist factor
    (with-current-buffer (get-buffer-create "*r4 authenticator*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Scan this QR code with your authenticator app, then type its code.\n\n")
        (when (and (display-images-p) (image-type-available-p 'svg))
          (insert-image (create-image .totp.qr_code 'svg t :background "white" :scale 1.5))
          (insert "\n\n"))
        (insert "Or add this key in the app:\n\n  " .totp.secret "\n\n" .totp.uri "\n"))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;;;###autoload (autoload 'r4-auth-setup-code "r4" nil t)
(defun r4-auth-setup-code ()
  "Set up an authenticator app (2FA) for the signed in account.
Show a QR code to scan with the app, then verify its first code.
radio4000.com admins need a code to moderate."
  (interactive)
  (r4-auth-require)
  (let ((factors (r4-auth--factors))
        (token (r4-auth-token)))
    (when (and (seq-find (lambda (factor) (equal (alist-get 'status factor) "verified")) factors)
               (not (yes-or-no-p "This account has an authenticator app already.  Add another? ")))
      (user-error "Use the app you have, with `r4-auth-verify-code'"))
    ;; A setup left unfinished blocks a new one.
    (dolist (factor factors)
      (unless (equal (alist-get 'status factor) "verified")
        (ignore-errors
          (r4-api-request "DELETE" (format "/auth/v1/factors/%s" (alist-get 'id factor))
                          :token token))))
    (let ((factor (r4-api-request "POST" "/auth/v1/factors" :token token
                                  :body `((factor_type . "totp")
                                          (friendly_name . ,(format-time-string "r4.el %F %T")))))
          (done nil))
      (unwind-protect
          (progn
            (r4-auth--show-qr-code factor)
            (r4-auth--verify-factor (alist-get 'id factor))
            (setq done t)
            (message "r4: authenticator app set up"))
        ;; The secret doesn't stay on screen, nor an unfinished setup.
        (when-let* ((buffer (get-buffer "*r4 authenticator*")))
          (kill-buffer buffer))
        (unless done
          (ignore-errors
            (r4-api-request "DELETE" (format "/auth/v1/factors/%s" (alist-get 'id factor))
                            :token token)))))))

;;;###autoload (autoload 'r4-whoami "r4" nil t)
(defun r4-whoami ()
  "Show the signed in radio4000.com user."
  (interactive)
  (if-let* ((session (r4-auth-session)))
      (message "Signed in to Radio4000 as %s" (plist-get session :email))
    (message "Not signed in to Radio4000, use `r4-login'")))

(provide 'r4-auth)

;;; r4-auth.el ends here
