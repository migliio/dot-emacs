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

(defun mg-kernel--do-grep (regexp)
  "Wrapper for `grep' to find definitions/usage of code
 snippets/structs in the kernel."
  (if-let ((kernel-directory (car (find-file-read-args "Select KERNEL SOURCE: " nil))))
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

(defun mg-kernel-do-grep ()
  "Prompt a regexp to grep in the kernel source."
  (interactive)
  (let ((regexp
	 (read-string "Provide a REGEXP to search: ")))
    (mg-kernel--do-grep regexp)))

(defun mg-kernel-coding-style/c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-add-style "linux-kernel"
			 '("linux" (c-offsets-alist
				    (arglist-cont-nonempty
				     c-lineup-gcc-asm-reg
				     linux-kernel-coding-style/c-lineup-arglist-tabs-only))))))

(defun mg-kernel-coding-style/setup ()
  (let ((filename (buffer-file-name)))
    ;; Enable kernel mode for the appropriate files
    (when (and buffer-file-name
               ( or (string-match "linux" buffer-file-name)
                 (string-match "liburing" buffer-file-name)))
      ;; (string-match "xfstests" buffer-file-name)))
      (setq indent-tabs-mode t)
      (setq tab-width 8)
      (setq c-basic-offset 8)
      (c-set-style "linux"))))

(add-hook 'c-mode-hook 'mg-kernel-coding-style/setup)

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
    (read-file-name "Kernel SOURCE: ")))
  (let* ((command (format "cd %s && make kernelversion" source))
	 (kernel-version
	  (shell-command-to-string command)))
    (if (string-match-p "\\<[0-9]+\\." kernel-version)
	(message "Kernel version for specificed SOURCE is %s" kernel-version)
      (user-error "Can't identify a kernel version for the specified SOURCE %s" source))))

;;; mg-kernel.el ends here
