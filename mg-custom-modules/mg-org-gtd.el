;;; mg-org-gtd.el --- GTD framework management within org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://git.sr.ht/~migliio/dot-emacs
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defcustom mg-org-gtd-file (expand-file-name "20231211T145832--inbox__gtd_personal.org" denote-directory)
  ""
  :type 'file)

(defun mg-org-gtd--set-header (&optional unset)
  "Set the local value of `header-line-format'.
With optional UNSET, unset the local value."
  (if unset
      (kill-local-variable 'header-line-format)
    (setq-local header-line-format
                (substitute-command-keys "This is mg-gtd clarify.  Start \\[mg-org-gtd-todo], abort \\[mg-org-gtd-abort]"))))

(defun mg-org-gtd--open-file ()
  "Return a buffer visiting `mg-org-gtd-file'."
  (or (get-file-buffer mg-org-gtd-file)
      (find-file-noselect mg-org-gtd-file)))

(defun mg-org-gtd--heading-motion (direction &optional count)
  (let ((dir (if (eq direction :backward)
                 'org-previous-visible-heading
               'org-next-visible-heading)))
    (widen)
    (funcall dir (or count 1))
    (if (eobp)
        ;; FIXME: This kills the buffer too early, so if there is
        ;; something to be done after we call `org-todo' (or anyhow,
        ;; our interactive function), it will not have time to
        ;; complete. We encountered this issue with the LOGBOOK entry.
        ;;
        ;; TODO: Experiment with adding this to some hook, like the
        ;; `org-after-todo-state-change-hook'.
        (kill-buffer (get-buffer "*mg-org-GTD*"))
      (org-narrow-to-subtree))))

;; (defvar mg-org-gtd-after-action-hook nil)

(defun mg-org-gtd-todo ()
  (interactive)
  (call-interactively #'org-todo)
  (call-interactively #'org-refile)
  (mg-org-gtd--heading-motion :forward)
  ;; (run-hooks 'mg-org-gtd-after-action-hook)
  )

(defun mg-org-gtd-abort ()
  (interactive)
  (if (bound-and-true-p mg-org-gtd-mode)
      (kill-buffer (get-buffer "*mg-org-GTD*"))
    (user-error "Not in a `mg-org-gtd-clarify' buffer")))

(defvar-keymap mg-org-gtd-mode-map
  :doc ""
  "C-c C-c" #'mg-org-gtd-todo
  "C-c C-k" #'mg-org-gtd-abort)

;; TODO: We may not even need this. Depending on what we do, we can
;; handle the state in a different way.
;;
;; NOTE: We do not want to use the start function here, because that
;; sets a permanent side effect for the buffer of origin (enables the
;; mode there). Whereas we want it in the indirect buffer only.
(define-minor-mode mg-org-gtd-mode
  ""
  :global nil
  :keymap mg-org-gtd-mode-map
  (unless (derived-mode-p 'org-mode)
    (user-error "Only enable this in an Org buffer"))
  (if mg-org-gtd-mode
      (mg-org-gtd--set-header)
    (mg-org-gtd--set-header :unset)))

(defun mg-org-gtd-start ()
  (interactive)
  (let* ((base-buffer (mg-org-gtd--open-file))
         (indirect-buffer (make-indirect-buffer base-buffer "*mg-org-GTD*" :preserve-original-state)))
    (with-current-buffer (pop-to-buffer indirect-buffer)
      (mg-org-gtd-mode 1)
      (widen)
      (goto-char (point-min)) ; TODO where should we start from?
      (unless (org-at-heading-p)
        (org-forward-heading-same-level 1))
      (org-narrow-to-subtree))))
