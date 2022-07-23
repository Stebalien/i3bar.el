;;; i3bar.el --- Display status from an i3status command in the tab bar -*- lexical-binding: t -*-

;; Copyright 2022 Steven Allen <steven@stebalien.com>

;; Author: Steven Allen <steven@stebalien.com>
;; URL: https://github.com/Stebalien/i3bar.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.0"))
;; Keywords:

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Displays the output of an i3status command in the Emacs mode-line (or tab-line).

;;; Code:

(require 'cl-macs)

(defgroup i3bar nil
  "i3bar"
  :version "0.0.1"
  :group 'mode-line)

(defcustom i3bar-command
  (seq-find 'executable-find '("i3status" "i3status-rs" "i3blocks") "i3status")
  "The i3status command."
  :group 'i3bar
  :type '(choice
          (string :tag "Shell Command")
          (repeat (string))))

(defcustom i3bar-separator "|"
  "The default block separator."
  :group 'i3bar
  :type '(choice
          (string :tag "Separator")
          (const :tag "None" nil)))

(defvar i3bar--process nil
  "The running i3bar process, if any.")

(defun i3bar--json-parse ()
  "Parse a json object from the buffer, or signal an error.
This is a thin wrapper around `json-parse-buffer', which changes the defaults."
  (json-parse-buffer :object-type 'plist :false-object nil))

(defvar i3bar-string ""
  "The i3bar string displayed in the mode-line.")

(put 'i3bar-string 'risky-local-variable t)

(define-minor-mode i3bar-mode
  "Display an i3bar in the mode-line."
  :global t :group 'i3bar
  (i3bar--stop)
  (or global-mode-string (setq global-mode-string '("")))
  (when i3bar-mode
    (unless (memq 'i3bar-string global-mode-string)
      (setq global-mode-string (append global-mode-string '(i3bar-string))))
    (i3bar--start)))

(defun i3bar--insert-block (block)
  (cl-destructuring-bind
      (&key (full_text "") color background (separator t) &allow-other-keys)
      block
    (let (face)
      (when color (setq face (plist-put face :foreground (substring color 0 -2))))
      (when background (setq face (plist-put face :background (substring background 0 -2))))
      (when face (set-text-properties 0 (length full_text) `(face ,face) full_text)))
    (when (and separator (length> i3bar-separator 0))
      (setq full_text (concat full_text i3bar-separator)))
    full_text))

(defun i3bar--update (update)
  "Apply an update to the i3status bar."
  (setq i3bar-string (mapconcat #'i3bar--insert-block update))
  (force-mode-line-update t))

(defun i3bar--process-filter (proc string)
  "Process output from the i3status process."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;; Write the input to the buffer (might be partial).
      (save-excursion
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      ;; Handle as much as we can.
      (condition-case err
          (while (progn (skip-chars-forward "[:space:]") (not (eobp)))
            (process-put
             proc 'i3bar-state
             (pcase (process-get proc 'i3bar-state)
               ;; Expect a protocol header
               ('nil (unless (= (plist-get (i3bar--json-parse) :version) 1)
                       (error "Version not supported"))
                     'begin)
               ;; Expect an opening [ of the infinite array.
               ('begin (unless (= (following-char) ?\[)
                         (error "Expected array of updates"))
                       (forward-char)
                       'update)
               ;; Expect an element in the infinite "update" array.
               ('update (if (= (following-char) ?\])
                            (progn (forward-char) 'end)
                          (i3bar--update (i3bar--json-parse))
                          'sep))
               ;; Expect a separator (comma).
               ('sep (unless (= (following-char) ?,)
                       (error "Expected a trailing comma"))
                     (forward-char)
                     'update)
               ;; We don't expect anything here, we should be done.
               ('end (error "Unexpected output")))))
        (json-parse-error) ; partial input, move on.
        ((error debug)     ; cleanup after a failure.
         (erase-buffer)
         (delete-process i3bar--process)
         (setq i3bar-string (format "i3bar failed: %s" err)
               i3bar--process nil)))
      (delete-region (point-min) (point)))))

(defun i3bar-reload ()
  "Restart the i3status program."
  (interactive)
  (unless i3bar-mode (user-error "The i3bar-mode is not enabled"))
  (i3bar--start))

(defun i3bar--start ()
  "Start the i3bar."
  (when (and i3bar--process (process-live-p i3bar--process))
    (delete-process i3bar--process)
    (setq i3bar--process nil))
  (with-current-buffer (get-buffer-create  " *i3bar input*")
    (erase-buffer)
    (setq i3bar--process (make-process
                          :name "i3bar"
                          :buffer (current-buffer)
                          :sentinel 'ignore
                          :stderr (messages-buffer)
                          :command (ensure-list i3bar-command)
                          :connection-type 'pipe
                          :noquery t
                          :filter #'i3bar--process-filter))))

(defun i3bar--stop ()
  "Stop the i3bar."
  (when (and i3bar--process (process-live-p i3bar--process))
    (delete-process i3bar--process)
    (setq i3bar-string ""
          i3bar--process nil)))

(provide 'i3bar)
;;; i3bar.el ends here
