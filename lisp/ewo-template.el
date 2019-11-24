;; Copyright 2017-2018 Éric Würbel

;; This file is part of EWO.

;; EWO is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; EWO is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EWO.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Easy Website with Org (EWO) project
;;;
;;; Generates a site organized in categories. Relies on the bootstrap
;;; framework by default but this can be customized.
;;;
;;; This file contains functions devoted to new template article
;;; generation management
(require 's)
(require 'ewo-util)
(require 'ewo-blog)

(defvar ewo--cat-history nil
  "History of categores (for use in minibuffer).")

(defvar ewo--template-history nil
  "History of templates (for use in minibuffer).")


(defun ewo--kw->tags (kwl &optional repl)
  "Transform a list of keywords into a list of tags. KWL is a
string containing keywords separated by a [, ] sequence. The
result is a string of tags surrounded by [:].

If a keyword contains any character in string REPL, il will be
replaced by an [_]. by default, REPL value is [ ']. REPL, when
put in [], should be a valid character class regular expression."
  (let* ((replace (concat "[" (if (null repl) " '" repl) "]"))
	 (lst     (split-string (s-trim kwl) " *, +" t))
	 (tags    (nlet loop ((l lst))
		    (if (null l)
			""
		      (concat ":" (replace-regexp-in-string replace "_" (car l))
			      (loop (cdr l)))))))
    (if (string= tags "") "" (concat tags ":"))))
    

  

;;;###autoload
(defun ewo-new-article (&optional cat filename title description keywords tags template)
  "Create a new article in the given category. CAT is the
category of the new article. FILENAME is the name of the file
containing the article. TITLE is the title of the article,
DESCRIPTION the description which will be inserted in the HTML
header. KEYWORDS are the keywords which will be insterted in the
HTML header. TAGS is the list of tags which will be used for
article indexing. TEMPLATE is the name of the template."
  (interactive
                                        ; category
   (let* ((default (car (car ewo-categories)))
          (c (completing-read (format "Category (%s) [%s]: "
                                      (mapconcat #'(lambda (var) (car var))
                                                 ewo-categories "/")
                                      default)
                              (mapcar #'(lambda (var) (car var)) ewo-categories)
                              nil t nil 'ewo--cat-history default))
          (catinfo	(assoc-string c ewo-categories))
          (is-blog 	(eq (plist-get (cdr catinfo) :type) 'blog))
          (f 		(read-string "File name: "))
          (tt 		(read-string "Title: "))
          (d 		(read-string "Description: "))
          (k 		(read-string "Keywords (separated by comma and space [, ]): "))
          (tg 		(when is-blog (read-string "Tags (surrounded by colons [:]): " (ewo--kw->tags k))))
          (templates 	(directory-files
			 (concat (file-name-as-directory ewo-root-dir)
				 (file-name-as-directory ewo-template-dir))
			 nil "^.+\\.org$"))
	  (def-tpl 	(if (null ewo--template-history) (car templates) (car ewo--template-history)))
          (tp 		(completing-read (format "Template [%s]: " def-tpl)
					 (mapcar #'(lambda (var) var) templates)
					 nil t nil 'ewo--template-history def-tpl)))
     (list c f tt d k tg tp)))
  (let ((path (concat
               (file-name-as-directory ewo-root-dir)
               (file-name-as-directory (plist-get (cdr (assoc-string cat ewo-categories)) :directory))
               filename)))
    (when (file-exists-p path)
      (user-error "File %s already exist" path))
    (let ((buf (find-file path)))
      (insert (format
               "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s
#+KEYWORDS:    %s
#+DESCRIPTION: %s
#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
"
               title
               (user-full-name)
               user-mail-address
               (format-time-string (car org-time-stamp-formats))
               (if (string= keywords "") "<TODO: insert your keywords here>" keywords)
               (if (string= description "") "<TODO: insert your description here>" description)
               org-export-default-language
               org-export-headline-levels
               nil ;; org-export-with-section-numbers
               nil ;; org-export-with-toc
               org-export-preserve-breaks
               ;; org-export-html-expand
               org-export-with-fixed-width
               org-export-with-tables
               nil ;; org-export-with-sub-superscripts
               nil ;; org-export-with-special-strings
               org-export-with-footnotes
               org-export-with-emphasize
               org-export-with-timestamps)
              (if tags
                  (format "#+FILETAGS:    %s\n"
                          (if (string= tags "") "<TODO: insert your tags here>\n" tags))

                "")
              (if (ewo--cat-is-blog-p cat)
                  "#+EWO_STATE:   unpublished"
                ""))
      (when (and template (not (string= template "")))
        (newline)
        (insert-file-contents (concat (file-name-as-directory ewo-root-dir)
                                      (file-name-as-directory ewo-template-dir)
                                      template))))))

(provide 'ewo-template)
