;;; mg-kernel.el --- Utilities to use with the Linux kernel -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

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
