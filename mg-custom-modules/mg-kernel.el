;;; mg-kernel.el --- Utilities to use with the Linux kernel -*- lexical-binding: t -*-

;; Copyright (C) 2025  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://crawlingaway.com/emacs/dot-emacs
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.4"))

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
;; This is a series of utility functions I use when working with the
;; Linux kernel. All of my activities lie into Emacs, and I really
;; enjoy writing Emacs Lisp code. Therefore, some of these function
;; could also be easily implemented as simple bash scripts. Writing
;; them in Lisp is merely a convenience for me working with Emacs
;; every day.

;;; Code:

;; First part of this library has been taken from . I found many of
;; these defaults useful when it comes to kernel development.

(defconst kernel-column-limit 80
  "It is only 80, get over it.")

(defvar kernel-source-path "~/sources/linux"
  "The kernel source path.")

(defvar kernel-comment-style 'extra-lines
  "Default style is 'extra-lines.
The another option is 'extra-bottom-line")
(make-variable-buffer-local 'kernel-comment-style)

(defvar kernel-lineup-tabs-only nil
  "If it is non-nil the kernel lineup indentation will make use of tabs only.
When nil lineup indentation will use TABS  SPACES.")
(make-variable-buffer-local 'kernel-lineup-tabs-only)

(defvar kernel-lineup-maximum-tabs nil
  "If it is non-nil its value will be the maximum tabs steps in kernel lineup
indentation.
When nil there will not be such a limit.
In both cases there is also the maximum limit forced by the
`kernel-lineup-arglist' function in conjuction with the
`kernel-column-limit' constant.")
(make-variable-buffer-local 'kernel-lineup-maximum-tabs)

(defun setup-kernel-style (comment-style lineup-tabs-only lineup-maximum-tabs)
  (setq kernel-comment-style comment-style)
  (setq kernel-lineup-tabs-only lineup-tabs-only)
  (setq kernel-lineup-maximum-tabs lineup-maximum-tabs))

(defun kernel-comment-dwim ()
  "Comments or uncomments the region.
If there's no active region adds/indents an one line comment or
comments/uncomments the current line if the one line comment is
empty."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (progn
          (setq beg (region-beginning)
                end (region-end))
          (if (comment-only-p beg end)
              (uncomment-region beg end)
            (progn
              (comment-region beg end)
              (save-excursion
                (goto-char beg)
                ;; Remove extra top line
                (when (equal kernel-comment-style 'extra-bottom-line)
                  (re-search-forward "/\\*\\s-*\n\\s-*\\(\\*\\)" end t)
                  (replace-match "/\\1"))
                ;; Update end point
                (goto-char beg)
                (re-search-forward "\\*/" nil t)
                (setq end (point))
                ;; This error is fixed in version 25.
                (when (< emacs-major-version 25)
                  (tabify beg end))
                ;; Cleaning only trailing spaces inserted by comment-region.
                ;; Existing ones are not touched.
                (goto-char beg)
                (while (re-search-forward
                        "\\(/\\|^\\s-\\)\\(\\*\\)\\(\\s-$\\)" end t nil)
                  (replace-match "\\1\\2")
                  (save-excursion
                    (re-search-forward "\\*/" nil t 1)
                    (setq end (point))))))))
      (progn
        (setq beg (line-beginning-position)
              end (line-end-position))
        (if (save-excursion
              (goto-char beg)
              (looking-at "\\s-*$"))
            (progn
              (comment-indent)
              (indent-according-to-mode))
          (if (comment-only-p beg end)
              (uncomment-region beg end)
            (if (save-excursion
                  (goto-char beg)
                  (re-search-forward "/\\*\\s-\\*/" end t 1))
                (progn
                  (save-excursion
                    (goto-char beg)
                    (let (kill-ring)
                      (comment-kill nil)))
                  ;; Read end position directly
                  (comment-region beg (line-end-position)))
              (comment-indent))))))))

(defun kernel-align-to-equals (begin end)
  "Align region to equal signs."
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 nil))

(defun kernel-lineup-arglist (langelem)
  "Line up the argument list for C function calls."
  (let* ((ret (c-lineup-arglist langelem))
         (anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (newcol (c-langelem-col langelem t))
         (steps (floor offset c-basic-offset)))
    (if (not kernel-lineup-tabs-only)
        ret
      (progn
        (when (>= ( newcol (* c-basic-offset steps))
                  kernel-column-limit)
          (setq steps (1- steps)))
        (when kernel-lineup-maximum-tabs
          (setq steps (min steps
                           kernel-lineup-maximum-tabs)))
        (* (max steps 1) c-basic-offset)))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-add-style
             "linux-kernel"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         kernel-lineup-arglist))
	       (c-cleanup-list brace-else-brace
			       brace-elseif-brace)))))
(defun kernel-style-hook ()
  (let ((filename (buffer-file-name))
        (source-path (expand-file-name kernel-source-path)))
    ;; Enable kernel mode for the appropriate files
    (when (and filename
               (string-match source-path filename))
      ;; Setup style
      (c-set-style "linux-kernel")
      (setq tab-width 8
            comment-style 'extra-line
            indent-tabs-mode t
            backward-delete-char-untabify-method nil)
      (c-toggle-auto-newline t)

      ;; Setup tree paths here
      (when (or (string-match (concat source-path "/net") filename)
                (string-match (concat source-path "/drivers/net") filename))
        (setup-kernel-style 'extra-bottom-line t nil))
      (when (string-match (concat source-path "/drivers/usb/host") filename)
        (setup-kernel-style 'extra-lines t 2))

      ;; Set kernel style key bindings
      (local-set-key [remap comment-dwim] 'kernel-comment-dwim)
      (local-set-key (kbd "C-c a =") 'kernel-align-to-equals)
      ;; Setup white space highlighting
      (require 'whitespace)
      (setq whitespace-line-column kernel-column-limit
            whitespace-style '(face empty
                                    indentation::tab
                                    whitespace-space-before-tab
                                    space-before-tab::tab
                                    lines-tail
                                    trailing))
      (dolist (face '(whitespace-line
                      whitespace-indentation
                      whitespace-space
                      whitespace-space-before-tab
                      whitespace-empty
                      whitespace-trailing))
        (set-face-background face "red"))
      (set-face-attribute whitespace-line nil
                          :background "red"
                          :foreground "yellow"
                          :weight 'bold)
      (whitespace-mode t))))

(add-hook 'c-mode-hook 'kernel-style-hook)

;; Then, the following code is brought by myself. Custom stuff I use
;; all the time.

(defun mg-kernel--get-source-directory ()
  "Get the kernel source based on the current file."
  (if-let ((root (vc-git-root (buffer-file-name))))
      root
    (car (find-file-read-args "Select KERNEL SOURCE: " nil))))

(defun mg-kernel--do-grep (regexp)
  "Wrapper for `grep' to find definitions/usage of code
 snippets/structs in the kernel."
  (if-let ((kernel-directory (mg-kernel--get-source-directory)))
      (let ((default-directory kernel-directory))
	(let* ((candidates
		(split-string
		 (shell-command-to-string
		  (format "find $1 -name '*.[ch]' | xargs grep -EnH \"%s\"" regexp))
		 "\n" t nil))
	       (parsed (split-string
			(completing-read "Select OCCUR to visit: " candidates)
			":" t nil)))
	  (with-current-buffer (find-file (expand-file-name (nth 0 parsed)))
	    (goto-line (string-to-number (nth 1 parsed))))))
    (user-error "Something went wrong when selecting KERNEL SOURCE")))

(defun mg-kernel--infer-regexp-to-search ()
  "Infer the regexp to search based on `point'.
When we search for usage of a certain C expression inside the
kernel source we do it because we are interested in what it's
under our cursor. Therefore, use the current symbol as a prompt
suggestion."
  (let* ((suggestion (symbol-at-point))
	(prompt (format "Provide a REGEXP to search [%s]: " suggestion)))
    (read-string prompt nil nil suggestion nil)))

(defun mg-kernel-do-grep ()
  "Prompt a regexp to grep in the kernel source."
  (interactive)
  (let ((regexp
	 (mg-kernel--infer-regexp-to-search)))
    (mg-kernel--do-grep regexp)))

(defun mg-get-kernel-version-from-source (source)
  "Return the kernel version from the source SOURCE.

When interactive, prompt the user for a kernel source. NOTE: This
function has a double check at the end, to be relatively sure
that a proper kernel version is returned to the user. This makes
sense because sometimes for a, e.g., mispell, the user provides a
wrong directory as SOURCE. The check could've been performed
better, but for now it just proved to work."
  (interactive
   (list
    (mg-kernel--get-source-directory)))
  (let* ((command (format "cd %s && make kernelversion" source))
	 (kernel-version
	  (shell-command-to-string command)))
    (if (string-match-p "\\<[0-9]+\\." kernel-version)
	(message "Kernel version for specificed SOURCE is %s" (string-trim kernel-version))
      (user-error "Can't identify a kernel version for the specified SOURCE %s" source))))

;;; mg-kernel.el ends here
