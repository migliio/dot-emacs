;;; mg-emacs.el --- Custom emacs functionalities -*- lexical-binding: t -*-

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
;; This library introduces some customizations within the Emacs text
;; editor. Its purpose its mainly to have some convenient
;; functionalities built on top of the Emacs base.

;;; Code:

(require 'mg-utils)

(defvar mg-screenshots-directory "~/.screenshots"
  "Directory for storing screenshots.

  This directory is used by the `mg-take-screenshot' function to
  store screenshots obtained with scrot.")

(defconst mg-scrot-command "/usr/bin/scrot -s "
  "This is the \"scrot\" command to use to take a screenshot and save it.")

(defun mg-insert-today-timestamp-formatted ()
  "Insert a timestamp of today at the current point.

  The timestamp is formatted around square brackets, which is the
  typical way I specify the date. The square bracketed date have a
  specific meaning in org-mode, but here we assume the date is
  inserted in a non-org file, or that it is not meaningful for
  agenda purposes."
  (interactive)
  (insert (format "[%s]" (mg-get-today-timestamp))))

(defun mg-take-screenshot ()
  "Take a screenshot using \"scrot\".

  The file will be saved under the `mg-screenshots-directory'. Since
  screenshots are meant to be further categorized, a \"RENAME\"
  string is added before the \"png\" extension."
  (interactive)
  (let ((screenshot-name (format "%s/%s" mg-screenshots-directory (format-time-string "%Y-%m-%d-%H-%M_screenshot_RENAME.png"))))
    (shell-command (concat mg-scrot-command screenshot-name))))

(defun mg-add-current-file-name-to-killring ()
  "Add the file name visited by the current buffer to the killring."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
    		    default-directory
    		  (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Added file name '%s' to the killring." filename))))

(defconst mg-get-battery-percentage-cmd "acpi -b | grep -E -o '[0-9][0-9][0-9]?%'"
  "This is a string representing a shell command to get the current battery percentage.")

(defconst mg-get-battery-status-cmd "acpi -b | grep -Eo 'Charging|Not charging|Discharging' | head -n 1"
  "This is a string representing a shell command to get the current battery status.")

(defconst mg-get-battery-remaining-time-cmd "acpi -b | grep -Eo '[0-9][0-9]:[0-9][0-9]:[0-9][0-9] %?'"
  "This is a string representing a shell command to get the current battery remaning time.")

(defconst mg-get-cpu-temp-cmd "sensors | grep 'Package id 0:\\|Tdie' | grep ':[ ]*+[0-9]*.[0-9]*°C' -o | grep '+[0-9]*.[0-9]*°C' -o"
  "This is a string representing a shell command to get the current CPU temperature.")

(defconst mg-get-cpu-usage-cmd "mpstat 1 1 | awk '/Average:/ {printf(\"%s\", $(NF-9))}'"
  "This is a string representing a shell command to get the current CPU usage.")

(defconst mg-get-total-ram-cmd "free -h | awk '/^Mem:/ {print $2}'"
  "This is a string representing a shell command to get the current total RAM capacity.")

(defconst mg-get-used-ram-cmd "free -h | awk '/^Mem:/ {print $3}'"
  "This is a string representing a shell command to get the current used RAM.")

(defconst mg-get-disk-usage-cmd "df -h / | awk '/\\//{ printf(\"%4s/%s\", $4, $2) }'"
  "This is a string representing a shell command to get the current usage of disk.")

(defconst mg-get-volume-percentage-cmd "amixer get Master | grep -oE '[0-9]{1,3}%' | head -n1"
  "This is a string representing a shell command to get the current volume level as percentage.")

(defconst mg-get-keyboard-layout-cmd "setxkbmap -query | grep layout | sed -e 's/layout:\s*//' -e 's/\s*$//'"
  "This is a string representing a shell command to get the current keyboard layout.")

(defconst mg-shutdown-system-cmd "/usr/sbin/shutdown now"
  "This is a string representing a shell command to shutdown the machine.")    

(defun mg--get-formatted-cpu-infos ()
  "Get all the battery-related information and format them."
  (format "%s @ %s"
    	(string-trim (shell-command-to-string mg-get-cpu-usage-cmd))
    	(string-trim (shell-command-to-string mg-get-cpu-temp-cmd))))

(defun mg--get-formatted-battery-infos ()
  "Get all the battery-related information and format them."
  (format "%s (%s) : %s"
  	(string-trim (shell-command-to-string mg-get-battery-percentage-cmd))
  	(string-trim (shell-command-to-string mg-get-battery-status-cmd))
  	(string-trim (shell-command-to-string mg-get-battery-remaining-time-cmd))))

(defun mg--get-formatted-ram-infos ()
  "Get all the battery-related information and format them."
  (format "%s @ %s"
    	(string-trim (shell-command-to-string mg-get-cpu-usage-cmd))
    	(string-trim (shell-command-to-string mg-get-cpu-temp-cmd))))

(defun mg--get-formatted-disk-infos ()
  "Get all the disk-related information and format them."
  (format "%s"
    	(string-trim (shell-command-to-string mg-get-disk-usage-cmd))))

(defun mg--get-formatted-volume-infos ()
  "Get all the volume-related information and format them."
  (format "%s"
    	(string-trim (shell-command-to-string mg-get-volume-percentage-cmd))))

(defun mg--get-formatted-keyboard-infos ()
  "Get all the keyboard-related information and format them."
  (format "%s"
    	(string-trim (shell-command-to-string mg-get-keyboard-layout-cmd))))

(defun mg-show-machine-info ()
"Show the some system information for the current machine.

  This function basically substitutes a full-fledged system-tray with additional information such as remaning battery time, memory and disk usage, etc. It better works in conjuction with a simple modeline and EXWM, and makes no sense when Emacs runs in a complete DE."
(interactive)
(let* ((battery-info (mg--get-formatted-battery-infos))
       (cpu-info (mg--get-formatted-cpu-infos))
       (ram-info (mg--get-formatted-ram-infos))
       (disk-info (mg--get-formatted-disk-infos))
       (volume-info (mg--get-formatted-volume-infos))
       (keyboard-layout-info (mg--get-formatted-keyboard-infos)))
  (message "System-level info => BAT: %s - CPU: %s - MEM: %s - DSK: %s - VOL: %s - KBD: %s"
    	 battery-info cpu-info ram-info disk-info volume-info keyboard-layout-info)))

(defun mg-shutdown-machine-with-confirmation
    "Ask for confirmation and shut down the system if confirmed."
  (when (yes-or-no-p "Are you sure you want to shut down the system? ")
    (shell-command mg-shutdown-system-cmd)))

(defun mg-new-buffer-with-mode ()
  "Create a new buffer and prompt the user for the major-mode to enable.

NOTE: as returned by auto-mode-alist cdr's, major modes are
strings. The `funcall' function takes a function canonical name
as input, then we should switch from string to canonical symbol
through `intern'."
  (interactive)
  (let* ((input-buff-name (read-string "Insert the buffer NAME: "))
       (new-buff (generate-new-buffer
  		  (if (string= "" input-buff-name)
  		      "untitled"
  		    input-buff-name)))
       (major-modes-list (mg--get-major-modes))
       (mode
  	(completing-read "Select the MAJOR MODE to enable: " major-modes-list))
       (mode-symbol (intern mode)))
    (switch-to-buffer new-buff)
    (if (fboundp mode-symbol)
      (funcall mode-symbol)
      (user-error "The selected mode doesn't exist or it is not lazily loaded"))
    (funcall (intern mode))
    new-buff))

;;; mg-emacs.el ends here
