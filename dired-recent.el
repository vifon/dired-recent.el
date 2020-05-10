;;; dired-recent.el --- Dired visited paths history     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wojciech Siewierski

;; Author: Wojciech Siewierski <wojciech dot siewierski at onet dot pl>
;; URL: https://github.com/vifon/dired-recent.el
;; Keywords: files
;; Version: 0.9
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple history keeping for dired buffers.  All the visited
;; directories get saved for reuse later.  Works great with Ivy and
;; other `completing-read' replacements.

;;  HOW TO USE IT:
;;
;;   (require 'dired-recent)
;;   (dired-recent-mode 1)
;;
;;  C-x C-d (`dired-recent-open')

;;; Code:

(require 'seq)

(defgroup dired-recent nil
  "Dired visited paths history."
  :group 'dired)

(defvar dired-recent-directories nil
  "List of the directories recently visited with `dired'.")

(defcustom dired-recent-directories-file (locate-user-emacs-file "dired-history")
  "File with the directories recently visited with dired."
  :type 'file)

(defcustom dired-recent-ignored-prefix nil
  "Directories ignored by `dired-recent-mode'.

A single string or list of strings.  Prefixes ignored by
`dired-recent-mode'.  Should include the trailing slash if the
prefix should be treated as a complete directory."
  :type '(repeat directory))

(defcustom dired-recent-max-directories nil
  "How many last directories should be remembered.

nil means to remember all."
  :type '(choice
          (const :tag "All" nil)
          (integer)))

(defvar find-program)
(defvar find-args)

;;;###autoload
(defun dired-recent-open ()
  "Show the dired history.  See: `dired-recent-mode'."
  (interactive)
  (unless dired-recent-directories
    (dired-recent-load-list))
  (let* ((label (completing-read "Dired recent: " dired-recent-directories))
         (res (or (get-text-property
                   0 'dired-recent-restore-file-list
                   ;; Get from original string stored in list, completing-read
                   ;; strips the properties.
                   (car (member label dired-recent-directories)))
                  label)))
    (cond ((functionp res)
           (funcall res nil nil)
           (when (equal (buffer-name) "*Find*")
             ;; Message command after finish.
             (let* ((proc (get-buffer-process (current-buffer)))
                    (sentinel (process-sentinel proc)))
               (set-process-sentinel
                proc
                (lambda (proc state)
                  (funcall sentinel proc state)
                  (message "%s Command was: %s %s"
                           (current-message)
                           find-program find-args))))))
          ((or (stringp res)
               (consp res))
           (dired res)))))

(defun dired-recent-ignored-p (path prefix)
  "Check if PATH starts with PREFIX and should be ignored by the dired history.

PREFIX is a list of paths that should not be stored in the dired history."
  (when prefix
    (or (string-prefix-p (car prefix) path)
        (dired-recent-ignored-p path (cdr prefix)))))

(defun dired-recent-path-save (&optional path)
  "Add current dired listing to `dired-recent-directories'.

PATH can be string or a list of format as first argument to
`dired'. If not given get listing info from current dired buffer.

Remove the last elements as appropriate according to
`dired-recent-max-directories'."
  (let ((path (or path dired-directory)))
    (unless (and (stringp path)
                 (dired-recent-ignored-p (file-name-as-directory path)
                                         dired-recent-ignored-prefix))
      (let ((new (cond ((get-buffer-process (current-buffer))
                        (let ((buf (current-buffer))
                              (label (format "%s:%s"
                                             (buffer-name)
                                             ;; Indicate process directory in
                                             ;; history name.
                                             (abbreviate-file-name
                                              default-directory))))
                          ;; `revert-buffer-function' is set after `dired-mode'
                          ;; call is finished, see `find-dired'.
                          (run-at-time
                           0 nil
                           (lambda ()
                             (when (buffer-live-p buf)
                               (put-text-property
                                0 1 'dired-recent-restore-file-list
                                (buffer-local-value
                                 'revert-buffer-function
                                 buf)
                                (copy-sequence label)))))
                          label))
                       ((consp path)
                        (let ((label (file-name-nondirectory
                                      (car path))))
                          (put-text-property
                           0 1 'dired-recent-restore-file-list
                           (cons (copy-sequence label)
                                 (cdr path))
                           label)
                          label))
                       (t path))))
        (setq dired-recent-directories
              (let ((new-list (cons new
                                    (delete new dired-recent-directories))))
                (if dired-recent-max-directories
                    (seq-take new-list dired-recent-max-directories)
                  new-list)))))))

;;; The default C-x C-d (`list-directory') is utterly useless. I took
;;; the liberty to use this handy keybinding.
(defvar dired-recent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-d") #'dired-recent-open)
    map))

(defun dired-recent-load-list ()
  "Load the dired history from `dired-recent-directories-file'."
  (interactive)
  (when (file-readable-p dired-recent-directories-file)
    (with-temp-buffer
      (insert-file-contents dired-recent-directories-file)
      (goto-char (point-min))
      (setq dired-recent-directories (read (current-buffer))))))

(defun dired-recent-cleanup ()
  "Remove nonexistent directories from `dired-recent-directories'.

Skips (preserves) the remote files as checking them would be
potentially slow."
  (interactive)
  (setq dired-recent-directories
        (seq-filter (lambda (x)
                      (or (file-remote-p x)
                          (file-directory-p x)))
                    dired-recent-directories)))

(defun dired-recent-save-list ()
  "Save the dired history to `dired-recent-directories-file'."
  (interactive)
  (with-temp-file dired-recent-directories-file
    (prin1 dired-recent-directories (current-buffer))))

;;;###autoload
(define-minor-mode dired-recent-mode
  "Toggle `dired-recent-mode' on or off.
Turn `dired-recent-mode' if ARG is positive, off otherwise.
Turning it on makes dired save each opened path."
  :keymap dired-recent-mode-map
  :global t
  :require 'dired-recent
  (if dired-recent-mode
      (progn
        (dired-recent-load-list)
        (add-hook 'dired-mode-hook #'dired-recent-path-save)
        (add-hook 'kill-emacs-hook #'dired-recent-save-list))
    (remove-hook 'dired-mode-hook #'dired-recent-path-save)
    (remove-hook 'kill-emacs-hook #'dired-recent-save-list)
    (dired-recent-save-list)))


(provide 'dired-recent)
;;; dired-recent.el ends here
