;;; mg-org.el --- Org-mode custom extensions -*- lexical-binding: t -*-

  ;; Copyright (C) 2024  Claudio Migliorelli

  ;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
  ;; URL: https://crawlingaway.org/emacs/dot-emacs
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
  ;; This library introduces some custom Emacs Lisp code to extend
  ;; `org-mode', which is one of the Emacs packages that I use the most.

  ;;; Code:

<<<<<<< HEAD
=======
  (defconst mg-work-projects-file (format "%s/%s" denote-directory "20231210T220334--work-and-study-projects__project_work.org"))
  (defconst mg-inbox-file (format "%s/%s" denote-directory "20231211T145832--inbox__gtd_personal.org"))
  (defconst mg-agenda-file (format "%s/%s" denote-directory "20231210T224321--agenda__personal.org"))
  (defconst mg-archive-file (format "%s/%s" denote-directory ".archive/archive.org"))
  (defconst mg-capture-notes-file (format "%s/%s" denote-directory "20231213T172757--capture-notes__gtd_personal.org"))
  (defconst mg-conferences-file (format "%s/%s" denote-directory "20231210T222135--conferences__personal_research.org"))
  (defconst mg-personal-projects-file (format "%s/%s" denote-directory "20231210T220139--personal-projects__personal_project.org"))
  (defconst mg-books-file (format "%s/%s" denote-directory "20240102T104309--books__personal_reading.org"))
  (defconst mg-planning-file (format "%s/%s" denote-directory "20240104T191508--planning__personal_planning.org"))
  (defconst mg-flashcards-file (format "%s/%s" denote-directory "20240220T165813--flashcards__learning_personal.org"))
  (defconst mg-reading-list-file (format "%s/%s" denote-directory "20241130T124328--references-reading-list__metanote_research.org"))
  (defconst mg-references-file (expand-file-name "20241204T192736--references__main_metanote_research.org" denote-directory))
  (defconst mg-latex-cmds '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

>>>>>>> 86e831e26c16300a9c64c0ce2b945871be2e12a7
  ;; Required for string-trim function
  (require 'subr-x)
  (defun mg-extract-heading-name (heading)
    "Extract the heading name, handling text before links, links, and task indicators."
    ;; Remove task progress indicators like [1/1] and trim trailing spaces
    (setq heading (string-trim (replace-regexp-in-string "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]\\s-*" "" heading)))
    ;; Function to extract and concatenate text before the link and the link description
    (let ((start 0) (parts '()))
      (while (string-match "\\(.*?\\)\\(\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]\\)" heading start)
        (push (match-string 1 heading) parts)  ; Text before the link
        (push (match-string 3 heading) parts)  ; Link description
        (setq start (match-end 0)))
      (push (substring heading start) parts)  ; Remaining text after last link
      (string-join (reverse parts) "")))

  (require 'mg-emacs-org)

  (defun mg-org-get-clock-minutes (file start-date end-date)
    "Get minutes from org-clock of a specific file and date."
    (interactive
     (list
      (read-file-name "Get clock data from FILE: ")
      (format "<%s>" (org-read-date))
      (format "<%s>" (org-read-date))))
    (let ((minutes (nth 1
  		      (with-current-buffer (find-file-noselect file)
  			(org-clock-get-table-data file `( :maxlevel 4
  							  :tstart ,start-date
  							  :tend ,end-date))))))
      minutes))

  (defun mg-org-get-tasks ()
    "Get active tasks from all `org-agenda-files'.

    This function returns a list of tasks taken from files belonging
    to the `org-agenda-files' list. NOTE: It needs to be tweaked and
    generalized to filter based on `org-todo-keywords'."
    (let ((tasks nil))
      (dolist (file org-agenda-files)
        (let* ((buffer-exists (get-file-buffer file))
    	     (buffer (or buffer-exists (find-file-noselect file))))
    	(with-current-buffer buffer
    	  (let ((task-list (save-excursion
    			     (org-agenda-get-todos))))
    	    (push task-list tasks)))
    	(unless buffer-exists
    	  (kill-buffer buffer))))
      (flatten-tree tasks)))

  (defun mg-org--task-prompt ()
    "Prompt the user for a task.

  The task can be selected from the list of tasks returned by
  `mg-org-get-tasks'."
    (completing-read "Select task: " (mg-org-get-tasks) nil :require-match))

  (defun mg-org-block-time ()
    "Prompt the user for time and task and block time.

  The user is continuously prompted with a date-time to select -
  the current day is supposed to be selected -, and it prompts the
  a list of pending org-agenda todo tasks. It creates and hidden
  file with the time-blocking and then it adds it to the
  `org-agenda-files'."
    (interactive)
    (let ((default-directory "/tmp/"))
      (with-current-buffer (find-file (format ".%s--timeblock.org" (format-time-string "%Y%m%dT%H%M%S")))
        ;; In case of one file, to avoid overwritting stuff:
        ;; (goto-char (point-max))
        (org-agenda-file-to-front)
        (catch 'no-time
    	(while t
    	  (let ((time (org-read-date)))
    	    (unless (string-match-p "[0-9-]\\{10\\} .*" time)
    	      (throw 'no-time time))
    	    (insert (format "* %s\n" (string-trim (mg-org--task-prompt))))
    	    (insert (format "SCHEDULED: <%s>\n\n" time))))))))


  (defvar my-org-export-functions
    '((html-buffer . org-html-export-as-html)
      (latex-buffer . org-latex-export-as-latex)))

  (defvar my-org-select-export-history nil)

  (defun my-org-select-export-function ()
    (let ((default (car my-org-select-export-history)))
      (intern
       (completing-read
        (format-prompt "Select export type" default)
        my-org-export-functions
        nil :require-match nil 'my-org-select-export-history
        default))))

  (defun my-org-export-get-function ()
    (alist-get
     (my-org-select-export-function)
     my-org-export-functions))

  (defun my-org-export-region-to-html (beg end export-fn)
    (interactive
     (list
      (region-beginning)
      (region-end)
      (my-org-export-get-function)))
    (unless (region-active-p)
      (user-error "No active region; aborting"))
    (let ((current-window (selected-window)))
      (unwind-protect
          (progn
            (narrow-to-region beg end)
            (funcall export-fn nil nil t t nil))
        (select-window current-window)
        (deactivate-mark)
        (widen))))

  (defun mg-org-capture-generate-flash-header ()
    "Generate the header to use in flaschards."
    (let ((link (mg-org--capture-get-last-file-link)))
      (format "%s @ %s" (mg-org--capture-get-last-file-link) (denote-get-identifier))))

  (defun mg-org--capture-get-last-file-link ()
    "In `org-capture' context, get last visited file's name and format as link."
    (let ((link (format "[[file:%s]]" (plist-get org-capture-plist :original-file))))
      link))

  (provide 'mg-org)
  ;;; mg-org.el ends here
