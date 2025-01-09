;;; mg-bib.el --- Extensions for bibliographic packages -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://crawlingaway.org/emacs/dot-emacs
;; Version: 0.0.7
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
;; This library introduces some extensions that I use to manage
;; bibliography files. The idea of this whole library is to leverage
;; org-mode to organize my bibliography.

;;; Code:

(require 'mg-org)
(require 'mg-denote)

(defun mg-bib-denote-org-capture-book-isbn-biblio ()
  "Custom `org-capture' template to add a book reference from a isbn code."
  (let ((isbn (mg-bib--denote-isbn-prompt)))
    (mg-bib-denote-org-capture-paper-biblio isbn)))

(defun mg-bib-denote-org-capture-website-biblio ()
  "Custom `org-capture' template to add a website reference."
  (let* ((url (read-string "URL: "))
    	 (title (mg-bib--www-get-page-title url))
    	 (authors (read-string "Insert authors: "))
    	 (date (org-read-date nil nil nil "Insert the article date: " nil nil nil))
	 (bibtex (format "@misc{%s,\nauthor = {%s},\ntitle = {%s},\nurl = {%s},\ndate = {%s},\nnote = {[Accessed %s]},\n}"
    			 "0000"
    			 authors
    			 title
    			 url
    			 date
    			 (format-time-string "%Y-%m-%d")))
	 (bibtex-list (mg-bib--bibtex-parse-entry bibtex)))
    (setq bibtex-list (push (format "@misc{%s"
				    (mg-bib--bibtex-generate-key bibtex-list))
			    (cdr bibtex-list)))
    (when-let* ((title (mg-bib--bibtex-get-field-content bibtex-list "title"))
    		(heading (format "* %s %s\n" title (mg-bib--denote-format-tags-as-org (mg-bib--denote-cycle-through-tags)))))
    (concat heading
    	    (mg-bib--denote-bibtex-org-block
	     (mg-bib--bibtex-list-to-string bibtex-list))))))

(defun mg-bib-denote-org-capture-paper-biblio (&optional isbn)
  "Custom `org-capture' template to add a paper/book reference."
  (let ((bibtex-input
         (if isbn
             (mg-bib--bibtex-parse-entry
	      (mg-bib--isbn-to-bibtex isbn))
           (mg-bib--bibtex-parse-entry
            (mg-bib--denote-bibtex-prompt)))))
    (when (not bibtex-input)
      (user-error "Couldn't retrieve the BibTeX entry."))
    (when-let*
	((key (mg-bib--bibtex-generate-key bibtex-input))
	 (bibtex-list (mg-bib--bibtex-refactor-entry-header bibtex-input key))
    	 (title (mg-bib--bibtex-get-field-content bibtex-list "title"))
    	 (heading (format "* %s %s\n" title 
			  (mg-bib--denote-format-tags-as-org (mg-bib--denote-cycle-through-tags))))
	 (file-path (mg-bib--denote-pull-resource-for-entry key)))
      (concat heading
    	      (mg-bib--denote-bibtex-org-block
	       (mg-bib--bibtex-list-to-string
		(mg-bib--bibtex-append-field bibtex-list "file" (mg-bib--bibtex-format-file-for-field file-path)))
	       file-path)))))

(defun mg-bib--denote-identifier-from-attrs (file)
  "Get the creation date of FILE from its attributes.
  Returns the creation date timestamp, otherwise nil."
  (let ((file-attrs (file-attributes file)))
    (if file-attrs
        (let ((creation-time (nth 6 file-attrs)))
          (if creation-time
              (format-time-string denote-id-format creation-time)
  	    nil)))))

(defun mg-bib--denote-pull-resource-for-entry (key)
  "Prompt the user for file path of paper having key KEY, format
       the file name and move it in the `denote-directory'."
  (let* ((file-path (read-file-name "Select a PDF file: "))
    	 (file-exists (file-exists-p file-path))
    	 (is-pdf (string-match-p "\\.pdf$" file-path)))
    (cond
     ((not file-exists)
      (user-error "Error: File does not exist."))
     ((not is-pdf)
      (user-error "Error: Selected file is not a PDF.")))
    (let* ((keywords (denote-keywords-prompt))
      	   (identifier (mg-bib--denote-identifier-from-attrs file-path))
      	   (new-file-name (format "%s--%s__%s" identifier key
      				  (mapconcat #'identity 
      					     (delete-dups (copy-sequence keywords))
      					     "_")))
      	   (new-file-path (format "%s/assets/%s.pdf" (denote-directory) new-file-name)))
      (rename-file file-path new-file-path)
      new-file-path)))

(defun mg-bib--bibtex-format-file-for-field (file-path)
  "Format FILE-PATH to be inserted as file field in the bibtex entry."
  (let ((file-name (car (last (split-string file-path "/")))))
    (format "%s:../pkm/assets/%s:application/pdf" file-name file-name)))

(defun mg-bib--bibtex-generate-key (bibtex-list)
  "Generate a bibtex key for BIBTEX-LIST.
The key format is author-title-year."
  (let ((author (or (mg-bib--bibtex-get-author-for-key bibtex-list) "noauthor"))
	(title (or (mg-bib--bibtex-get-title-for-key bibtex-list) ""))
	(year (or (mg-bib--bibtex-get-year-for-key bibtex-list) "nodate")))
    (format "%s_%s_%s" author title year)))

(defun mg-bib--bibtex-get-author-for-key (bibtex-list)
  "Get the author from BIBTEX-LIST, formatted and ready to be used
in the bibtex key."
  (let* ((first-author (string-trim
			(car (string-split
			      (downcase (mg-bib--bibtex-get-field-content bibtex-list "author")) "and"))))
	 (author-tag
	  (if (string-match "," first-author)
	      (car (string-split first-author ","))
	    (car (cdr (string-split first-author " "))))))
    ;; NOTE: In case there was no way of getting the surname, just
    ;; grab the whole author string.
    (if author-tag
	(mg-bib--bibtex-sanitize-author-for-key author-tag)
      (mg-bib--bibtex-sanitize-author-for-key first-author))))

(defun mg-bib--bibtex-sanitize-author-for-key (surname)
  "Perform a sanitization over the author's surname to generate
 the bibtex key."
  ;; This is required because sometimes we have "John Jr., Doe". Then,
  ;; producing "john jr." as surname for the key to be generated is
  ;; WRONG. Moreover, sometimes there are "-" signs in author keys,
  ;; e.g., Anthropic-AI. Therefore, remove those.
  (let ((sanitized-surname (car (split-string surname " "))))
    (replace-regexp-in-string "-" "" sanitized-surname)))

(defun mg-bib--bibtex-get-year-for-key (bibtex-list)
  "Extract year from BIBTEX-LIST entry for use in BibTeX key.
Looks first in the 'year' field, then in the 'date' field.
Returns the first 4-digit year (between 1000-2999) found or signals an error."
  (let* ((year (mg-bib--bibtex-get-field-content bibtex-list "year"))
         (date (mg-bib--bibtex-get-field-content bibtex-list "date"))
         (year-regexp "\\b\\(1[0-9][0-9][0-9]\\|2[0-9][0-9][0-9]\\)\\b"))
    (cond
     ((and year
           (string-match year-regexp year))
      (match-string 1 year))
     ((and date
           (string-match year-regexp date))
      (match-string 1 date))
     (t
      (user-error "No valid year found in 'year' or 'date' fields")))))

(defun mg-bib--bibtex-get-type (bibtex-list)
  "Get the entry type for BIBTEX-LIST."
  (let ((field (car bibtex-list)))
    (when (string-match "^@\\(\\w+\\){\\([^,]+\\)" field)
      (let ((type (downcase (match-string 1 field))))
	type))))

(defun mg-bib--bibtex-refactor-entry-header (bibtex-list key)
  "Refactor BIBTEX-LIST's header to be downcase and with custom
 KEY."
  (let ((type (mg-bib--bibtex-get-type bibtex-list)))
    (push (format "@%s{%s" type key) (cdr bibtex-list))))

(defun mg-bib--bibtex-sanitize-title-for-key (title)
  "Perform a sanitization over the title to generate the bibtex key."
  (let ((formatted-title nil))
    (setq formatted-title (replace-regexp-in-string "-" "" title))
    (setq formatted-title (replace-regexp-in-string "'" "" formatted-title))
    (setq formatted-title (replace-regexp-in-string "." "" formatted-title))
    (setq formatted-title (replace-regexp-in-string ":" "" formatted-title))
    (setq formatted-title (replace-regexp-in-string "/" "" formatted-title))
    formatted-title))

(defun mg-bib--bibtex-get-title-for-key (bibtex-list)
  "Get the title from BIBTEX-LIST, formatted and ready to be used
in the bibtex key."
  (when-let* ((title-early (mg-bib--bibtex-get-field-content bibtex-list "title"))
	      ;; NOTE: Sometimes titles have `'`, `:`, etc. signs in
	      ;; them. Let's rule those diacritics out.
	      (title (mg-bib--bibtex-sanitize-title-for-key title-early)))
    (downcase (car (split-string title " ")))))

(defun mg-bib--denote-bibtex-prompt (&optional default-bibtex)
  "Ask the user for a bibtex entry. Returns the sanitised
          version. See `mg-bib--denote-sanitise-bibtex' for details."
  (let* ((def default-bibtex)
      	 (format (if (and def (not (string-empty-p def)))
      		     (format "Bibtex [%s]: " def)
      		   "Bibtex: "))
      	 (sanitised-bibtex (read-string format nil nil def)))
    (if sanitised-bibtex
      	sanitised-bibtex
      (user-error "Invalid BiBTeX entry provided to `mg-bib--denote-bibtex-prompt'"))))

(defun mg-bib--denote-isbn-prompt ()
  "Ask the user for a isbn entry."
  (let ((isbn (read-string "ISBN: ")))
    isbn))

(defun mg-bib--denote-bibtex-org-block (bibtex &optional file)
  "Returns a string representing an org `bibtex' source block
          encompassing BIBTEX, a string of a bibtex entry."
  (let* ((src
          (format "#+begin_src bibtex :tangle \"%s\"\n%s\n#+end_src" mg-bibliography-path bibtex))
         (entries (format
          	   ":PROPERTIES:\n:FILE: %s\n:NOTES:\n:END:\n"
		   (if file
		       (mg-denote-generate-link-from-file-path file)
		     ""))))
    (format "%s\n%s\n" entries src)))

(defun mg-bib--bibtex-early-sanitize (bibtex)
  "Perform some early initialization on BIBTEX."
  (replace-regexp-in-string "[ \t]+=" " =" (replace-regexp-in-string "\n[ \t]+" "\n" (replace-regexp-in-string "[ \t]+\n" "\n" bibtex))))

(defun mg-bib--bibtex-parse-entry (bibtex)
  "Parse BIBTEX to make a list.
Each entry is a bibtex field with a value."
  (let* ((bibtex-fields
	  (string-split (mg-bib--bibtex-early-sanitize bibtex) ",$"))
	 (bibtex-list
	  (mapcar (lambda (string)
		    (string-trim (replace-regexp-in-string "\n" " " string)))
		  bibtex-fields))
	 (bibtex-final (reverse bibtex-list))
	 (before-check (nreverse
			(append (list (setcar bibtex-final (replace-regexp-in-string "}.*?}" "}" (car (last bibtex-list)))))
				(cdr bibtex-final)))))
    (if (and before-check (string= "}" (car (last before-check))))
	(butlast before-check)
      before-check)))

(defun mg-bib--bibtex-get-field (bibtex-list field)
  (seq-find (lambda (string)
	      (string-match-p (format "%s.*?=" field) string)) bibtex-list))

(defun mg-bib--bibtex-get-field-content (bibtex-list field)
  (when-let ((raw-field (mg-bib--bibtex-get-field bibtex-list field)))
    (when (string-match (format "%s\\s-*=[[:space:]]*{\\([^}]*\\)}" field) raw-field)
      (match-string 1 raw-field))))

(defun mg-bib--bibtex-list-to-string (bibtex-list)
  "Reformat BIBTEX-LIST as a bibtex entry (string)."
  (let ((closed-list
	 (mg-bib--bibtex-close-entry bibtex-list)))
    (mapconcat #'identity closed-list ",\n")))

(defun mg-bib--bibtex-close-entry (bibtex-list)
  "Close the bibtex entry represented by BIBTEX-LIST."
  (nreverse
     (append (list "}") (nreverse bibtex-list))))

(defun mg-bib--bibtex-append-field (bibtex-list field value)
  "Append FIELD with value VALUE to BIBTEX-LIST."
  (let ((field-formatted (format "%s = {%s}" field value)))
    (add-to-list 'bibtex-list field-formatted :append nil)))

(defun mg-bib--bibtex-list-find-field-and-replace (bibtex-list field replace)
  "Find and replace a specific field in a BibTeX entry list."
  (mapcar (lambda (entry)
            (if (string-match (format "%s.*?=" field) entry)
                (replace-regexp-in-string 
                 (format "%s.*?= {[^}]*}" field)
                 (format "%s = {%s}" field replace)
                 entry)
              entry))
          bibtex-list))

(defun mg-bib--denote-cycle-through-tags ()
  "Cycle through tags in the references file prompting the user for an input."
  (let* ((tags (mg-org-get-tags-from-file mg-references-file))
	 (selected-tags (completing-read-multiple "Select tags: " tags)))
    (sort selected-tags #'string<)))

(defun mg-bib--denote-format-tags-as-org (tags-list)
  "Format TAGS-LIST as a series of org tags."
  (concat ":" (mapconcat #'identity tags-list ":") ":"))

(defun mg-bib--field-empty-p (entry)
  "Return t if ENTRY contains only empty braces (i.e. '{}'), nil otherwise.
ENTRY should be a string containing a BibTeX field value."
  (string-match-p "^[[:space:]]*{[[:space:]]*}[[:space:]]*$" entry))

(defun mg-bib--bibtex-from-isbn-sanitize (bibtex)
  "Sanitize BIBTEX, assuming that it has been generated by a ISBN."
  (when (mg-bib--field-empty-p bibtex)
    (kill-new bibtex)
    (user-error "The BibTeX entry is malformed, it has been copied to the killring.")))

(defun mg-bib--isbn-to-bibtex (isbn)
  "Given ISBN for a book, generate a BibTeX entry.
This code is adapted from the one developed by John Kitchin for org-ref."
  (let* ((url (format "https://openlibrary.org/isbn/%s.json" isbn))
         (json (with-current-buffer (url-retrieve-synchronously url)
                 (json-read-from-string (string-trim (buffer-substring url-http-end-of-headers (point-max))))))
	 (title (cdr (assoc 'title json)))
	 (publishers-list (cdr (assoc 'publishers json)))
	 (publisher (mapconcat #'identity publishers-list ", "))
	 (date (cdr (assoc 'publish_date json)))
	 (author-urls (cdr (assoc 'authors json)))
	 (authors (mapconcat
		   #'identity
		   (cl-loop for aurl across author-urls
			    collect
			    (with-current-buffer (url-retrieve-synchronously
						  (format "https://openlibrary.org%s.json"
							  (cdr (assoc 'key aurl))))
			      (cdr (assoc 'personal_name
					  (json-read-from-string
					   (string-trim (buffer-substring url-http-end-of-headers (point-max))))))))
		   " and "))
	 (burl (format "https://openlibrary.org/%s" (cdr (assoc 'key json))))
	 (bibtex (format "@book{,
  author = {%s},
  title = {%s},
  publisher = {%s},
  date = {%s},
  url = {%s}
}"
			 authors
			 title
			 publisher
			 date
			 burl)))
    bibtex))

(defun mg-bib-kill-bibtex-from-isbn ()
  "Prompt a ISBN interactively and kill the generated BibTeX entry."
  (interactive)
  (when-let ((bibtex (mg-bib--isbn-to-bibtex
		      (mg-bib--denote-isbn-prompt))))
    (kill-new bibtex)))

(defun mg-bib--www-get-page-title (url)
  "Get the page title from URL."
  (let ((title))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
      (setq title (match-string 1)))
    title))

(defun mg-bib--denote-prompt-and-return-point ()
  "Prompt the user for org headings in `mg-references-file' and return the point related to the notes part of the selected heading."
  (save-excursion
    (with-current-buffer (find-file-noselect mg-references-file)
      (let* ((current-pos (point))
	     (headings (org-map-entries
			(lambda ()
			  (cons (org-get-heading t t t t)
				(point)))
			t 'file))
	     (headings-names (mapcar #'car headings))
	     (selected (completing-read "Select heading: " headings-names nil t))
	     (heading-pos (cdr (assoc selected headings))))
	(if (not heading-pos)
	    (error "Heading not found")
	  (goto-char heading-pos)
	  (let ((end-of-subtree (save-excursion
				  (org-end-of-subtree t t))))
            (if (re-search-forward "^[ \t]*#\\+end_src" end-of-subtree t)
		(progn
                  (end-of-line)
                  (let ((target-pos (point)))
		    target-pos))
	      (error "No source block found in the selected heading"))))))))

(defun mg-bib-denote-goto-notes-interactively ()
  "Prompt the user for headings in `mg-references-file' and go to the notes section of the selected heading. The subtree is then narrowed for convenience."
  (interactive)
  (let ((point (mg-bib--denote-prompt-and-return-point)))
    (with-current-buffer (find-file mg-references-file)
      (goto-char point)
      (org-reveal)
      (org-narrow-to-subtree))))

(provide 'mg-bib)
;;; mg-bib.el ends here
