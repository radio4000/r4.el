;;; r4-test-helpers.el --- Helpers of the r4 tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'ert)
(require 'r4)

;; Never read or write the files of the user.
(setq r4-history-file nil
      r4-channels-cache-file nil
      r4-auth-session-file nil)

(defun r4-test--track (&rest fields)
  "Return an `r4-track' with FIELDS and default values."
  (apply #'r4-track-create
         (append fields (list :id "9919e967-db35-459b-b3dd-b9f439d4b827"
                              :slug "test" :title "A track"
                              :url "https://www.youtube.com/watch?v=pmpXABDFMyE"))))

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

(defmacro r4-test--with-answers (answers &rest body)
  "Run BODY, answering the minibuffer prompts with ANSWERS, in order."
  (declare (indent 1))
  `(let ((answers ,answers))
     (cl-letf (((symbol-function 'read-string)
                (lambda (&rest _) (or (pop answers) (error "No more answers")))))
       ,@body)))

(provide 'r4-test-helpers)

;;; r4-test-helpers.el ends here
