;;; mg-modeline.el --- Custom modeline for the Emacs editor -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://crawlingaway.org/emacs/dot-emacs
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
;; This source file defines a custom modeline for the Emacs editor. It
;; has a cleaner structure compared to the original one, and
;; highlights buffer state (i.e., local vs. remote, saved vs. unsaved)
;; more clearly.

;;; Code:

(defvar mg-modeline-buffer-status
    '(:eval
      (if (file-remote-p default-directory)
	  (propertize " @ "
		      'mouse-face 'mode-line-highlight)
	(propertize " ^ "
		    'mouse-face 'mode-line-highlight)))
  "This is the propertized variable containing the status of the
current buffer. The status specifies whether this buffer is
remote or local.")

(defvar mg-modeline-buffer-name
    '(:eval
      (propertize (buffer-name) 'face 'mode-line-buffer-id))
    "This is the propertized variable containing the current buffer's
name.")

(defun mg-modeline--major-mode ()
  "This is a function used to get the current major mode for the
opened buffer. The major mode is obtained by looking at the
`major-mode' variable. In case we are under EXWM, the major mode
haas also a `exwm--input-mode' further specification (i.e., Char
or Line) that is also useful to specify in the modeline."
  (let* ((mode
	 (mapconcat 'capitalize
		    (butlast (split-string (symbol-name major-mode) "-")) " "))
	 (mode-final
	  (if (string-equal mode "Exwm")
	      (concat "Exwm: "
	       (mapconcat 'capitalize
			  (butlast (split-string (symbol-name exwm--input-mode) "-"))))
	    mode))
	 (indicator (cond
		     ((derived-mode-p 'text-mode) "§")
		     ((derived-mode-p 'prog-mode) "λ")
		     ((derived-mode-p 'comint-mode) ">_")
		     (t "o"))))
	  (format "%s (%s)" indicator mode-final)))

(defvar mg-modeline-major-mode
    '(:eval
      (mg-modeline--major-mode))
      "This is the variable containing the major mode for the current
 buffer, as returned by the `mg-modeline--major-mode' function")

(defvar mg-modeline-buffer-mode
    '(:eval
      (if buffer-read-only
	    "*L*"
	"*U*"))
    "This is the variable indicating whether the buffer is in
 read-only mode or not." )

(setq mode-line-end-spaces
      '(""
	mode-line-misc-info))

(setq-default mode-line-format
      '("%e"
	mg-modeline-buffer-status
	mg-modeline-buffer-mode
	" "
	mg-modeline-buffer-name
	" "
	mode-line-position
	"  "
	(vc-mode vc-mode)
	"  "
	mg-modeline-major-mode
	"  "
	(:eval
	 (when (mode-line-window-selected-p)
	   mode-line-end-spaces))))

(dolist (construct
	 '(mg-modeline-major-mode
	   mg-modeline-buffer-mode
	   mg-modeline-buffer-status
	   mg-modeline-buffer-name))
  (put construct 'risky-local-variable t))

(provide 'mg-modeline)
;;; mg-modeline.el ends here
