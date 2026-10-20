;;; r4-mpv.el --- Control mpv over its JSON IPC  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One idle mpv process, controlled through its JSON IPC socket: send
;; commands, read properties, and receive events, see
;; https://mpv.io/manual/stable/#json-ipc.

;;; Code:

(require 'cl-lib)

(defvar r4-mpv-event-functions nil
  "Functions called with each mpv event, an alist.")

(defvar r4-mpv-exit-hook nil
  "Hook run when mpv exits.")

(defvar r4-mpv--process nil "The mpv process.")
(defvar r4-mpv--ipc nil "The network process connected to mpv.")
(defvar r4-mpv--requests (make-hash-table) "Callbacks of the requests, by id.")
(defvar r4-mpv--request-id 0 "Id of the last request.")

(defun r4-mpv--socket ()
  "Return the file name of the IPC socket."
  (expand-file-name (format "r4-mpv-%d.sock" (emacs-pid)) temporary-file-directory))

(defun r4-mpv-live-p ()
  "Return non-nil if mpv is running."
  (and (process-live-p r4-mpv--process) (process-live-p r4-mpv--ipc)))

(defun r4-mpv-start (program arguments)
  "Start PROGRAM, mpv, with ARGUMENTS, and connect to its IPC socket."
  (unless (executable-find program)
    (user-error "Cannot find `%s', please install it" program))
  (r4-mpv-kill)
  (let ((socket (r4-mpv--socket)))
    (ignore-errors (delete-file socket))
    (setq r4-mpv--process
          (make-process
           :name "r4-player"
           :buffer " *r4-player*"
           :command `(,program "--idle=yes" "--no-terminal"
                               ,(concat "--input-ipc-server=" socket)
                               ,@arguments)
           :noquery t
           :sentinel #'r4-mpv--sentinel))
    (with-timeout (5 (r4-mpv-kill) (error "Timeout waiting for mpv to start"))
      (while (not (file-exists-p socket))
        (accept-process-output r4-mpv--process 0.05)))
    (setq r4-mpv--ipc
          (make-network-process
           :name "r4-player-ipc"
           :family 'local
           :service socket
           :coding 'utf-8
           :noquery t
           :filter #'r4-mpv--filter))))

(defun r4-mpv-kill ()
  "Stop mpv and forget the connection."
  (dolist (process (list r4-mpv--ipc r4-mpv--process))
    (when (process-live-p process) (delete-process process)))
  (setq r4-mpv--ipc nil r4-mpv--process nil)
  (clrhash r4-mpv--requests))

(defun r4-mpv--sentinel (process _event)
  "Clean up when the mpv PROCESS exits."
  (unless (process-live-p process)
    (when (eq process r4-mpv--process)
      (r4-mpv-kill)
      (run-hooks 'r4-mpv-exit-hook))))

(defun r4-mpv-send (&rest command)
  "Send COMMAND, a list of arguments, to mpv."
  (process-send-string r4-mpv--ipc
                       (concat (json-serialize `((command . ,(vconcat command)))) "\n")))

(defun r4-mpv-get-property (name callback)
  "Call CALLBACK with the value of the property NAME, or nil."
  (when (r4-mpv-live-p)
    (let ((id (cl-incf r4-mpv--request-id)))
      (puthash id callback r4-mpv--requests)
      (process-send-string
       r4-mpv--ipc
       (concat (json-serialize `((command . ["get_property" ,name]) (request_id . ,id))) "\n")))))

(defun r4-mpv--filter (process output)
  "Handle the JSON messages in OUTPUT sent by mpv on PROCESS."
  (let ((lines (split-string (concat (process-get process 'r4-pending) output) "\n")))
    (process-put process 'r4-pending (car (last lines)))
    (dolist (line (butlast lines))
      (unless (string-empty-p line)
        (r4-mpv--handle (json-parse-string line :object-type 'alist
                                           :null-object nil :false-object nil))))))

(defun r4-mpv--handle (message)
  "Handle MESSAGE from mpv: a reply to a request, or an event."
  (let-alist message
    (if-let* ((callback (and .request_id (gethash .request_id r4-mpv--requests))))
        (progn (remhash .request_id r4-mpv--requests)
               (funcall callback (and (equal .error "success") .data)))
      (when .event
        (run-hook-with-args 'r4-mpv-event-functions message)))))

(provide 'r4-mpv)

;;; r4-mpv.el ends here
