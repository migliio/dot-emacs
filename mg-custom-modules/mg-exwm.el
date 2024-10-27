;;; mg-exwm.el --- Utility functions for EXWM -*- lexical-binding: t -*-

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
;; This library includes some utility functions that I use when EXWM
;; is enabled.

;;; Code:

(defconst mg-exwm--screenlayout-dir "~/.screenlayout"
  "Directory for screenlayouts to be used with randr.")

(defconst mg-exwm--zurich-layout-script-name "zurich.sh"
  "Script to enable the layout for the workstation in Zurich.")

(defconst mg-exwm--default-layout-script-name "default.sh"
  "Script to enable the default layout.")

(defconst mg-exwm--xrandr-think-name "eDP1"
  "Name assigned by xrandr to the screen of \"think\".")

(defun mg-exwm--change-layout (script)
  "Change the current xrandr layout by executing SCRIPT."
  (start-process-shell-command "screenlayout" nil
			       (format "%s/%s" mg-exwm--screenlayout-dir script)))

(defun mg-exwm-trigger-default-layout ()
  "Trigger the default layout."
  (interactive)
  (mg-exwm--change-layout mg-exwm--default-layout-script-name))

(defun mg-exwm-trigger-zurich-layout ()
  "Trigger layout for the workstation in Zurich."
  (interactive)
  (mg-exwm--change-layout mg-exwm--zurich-layout-script-name)
  ;; NOTE: The Zurich layout has two screens, therefore move
  ;; workspaces 2 and 5 to second screen
  (setq exwm-randr-workspace-monitor-plist `(2 ,mg-exwm--xrandr-think-name 5 ,mg-exwm--xrandr-think-name))
  (exwm-randr-refresh))

(provide 'mg-exwm)
;;; mg-exwm.el ends here
