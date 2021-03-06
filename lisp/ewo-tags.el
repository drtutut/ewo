;; Copyright 2017-2019 Éric Würbel

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
;;; This file contain tags processing functions

(require 'avl-tree)
(require 'ewo-util)

(defun ewo--tagtree-cmpfunc (elt1 elt2)
  (string< (car elt1) (car elt2)))

(defun ewo--tagtree-updatefunc (new match)
  (cons (car new) (cons (cadr new) (cdr match))))

(defun ewo--add-to-tag-map (tag title file)
  "a reference to file in the tag entry of the `ewo--tags' table."
  (avl-tree-enter ewo--tags (list tag (cons file title)) 'ewo--tagtree-updatefunc))

(defun ewo--tagfile-header (&optional tagname rep-underscore)
  "Generates the header of the tag file. If optionnal aggument
TAGNAME is provided, the function generates the index for this
tag, otherwise it consider that this is the global tags.org
file.

If REP-UNDERSCORE is specified, it represent a string which replaces any underscore in the tag name.
"
  (insert "#+TITLE: "
          (if tagname
	      (if rep-underscore (replace-regexp-in-string "_" rep-underscore tagname) tagname)
	    "Tags"))
  (newline)
  (insert "#+DATE: ")
  (org-time-stamp '(16))
  (newline)
  (insert "#+OPTIONS: toc:nil num:nil")
  (newline)
  (insert "#+DESCRIPTION: tag index for "
          (if tagname
	      (if rep-underscore (replace-regexp-in-string "_" rep-underscore tagname) tagname)
	    (concat "site " ewo-name)))
  (newline 2))

(defun ewo--tagfile-tag-content (tag)
  "Generates the content of the tagfile for tag TAG in the
current buffer."
  (dolist (pair (cdr tag))
    (let ((file (car pair))
          (title (cdr pair)))
      (insert "- [[file:../" file "][" title "]]")
      (newline))))

(defun ewo--gen-tagfile (file tag)
  "Generate the tag FILE for TAG."
  (save-excursion
    (let* ((visiting (find-buffer-visiting file))
           (buffer (or visiting (find-file-noselect file))))
      (unwind-protect
          (with-current-buffer buffer
            (erase-buffer)
            (ewo--tagfile-header (car tag) " ")
            (ewo--tagfile-tag-content tag)
            (save-buffer))
        (unless visiting (kill-buffer buffer))))))

(defun ewo--tagfile (tag &optional ext)
  "Compute the filename corresponding to a tag.  TAG is the name
of the tag. EXT is the extention. if nil, \".org\" is assumed"
                                        ; simple solution (see later
                                        ; if something more robust is
                                        ; needed, but I don't thik so,
                                        ; because allowed chars in org
                                        ; tags are already
                                        ; [a-zA-Z0-9_@]
  (when (string-match "^[[:word:]0-9_@]+$" tag)
    (concat tag (if (and ext (stringp  ext)) ext ".org"))))

(defvar ewo--tag-max-ref 1
  "Number of references of the tag having the greatest number of
references.")

(defun ewo--compute-tag-max-ref ()
  "Return the number of references of the tag having the greatest
number of references."
  (message "############# ZORGL")
  (setq ewo--tag-max-ref (apply 'max (avl-tree-mapcar (lambda (e) (length (cdr e))) ewo--tags))))

(defun ewo--tag-size (refs maxrefs minsize maxsize)
  "Compute the size (in em units) of a tag having a REFS count.
MAXREFS is the greatest number of references, MINSIZE the
smallest acceptable size (in em unit), MAXSIZE the biggest
acceptable size (in em units).

The computation method depends of the value of `ewo-tag-sizing'."
  (cond
   ((eq ewo-tag-sizing 'linear)
    (/ (+ (* minsize (- maxrefs refs)) (* maxsize (- refs 1))) (- maxrefs 1)))
   ((eq ewo-tag-sizing 'log)
					; log regression
    (let* ((mean-y    (/ (+ minsize maxsize) 2))
	   (mean-lnx  (/ (+ (log 1) (log maxrefs)) 2))
	   (sxx       (- (+ (* (log 1) (log 1)) (* (log maxrefs) (log maxrefs))) (* 2 (* mean-lnx mean-lnx))))
	   (sxy       (- (+ (* (log 1) minsize) (* (log maxrefs) maxsize)) (* 2 (* mean-lnx mean-y))))
	   (b         (/ sxy sxx))
	   (a         (- mean-y (* b mean-lnx))))
      (message "a = %f" a)
      (message "b = %f" b)
      (+ a (* b (log refs)))))
   (t 1)))

(defun ewo--process-tag (tag &optional rep-underscore)
  "Generate an entry in the tag index (current buffer).
Also generate the file corresponding to the tag TAG.

If specified, REP-UNDERSCORE is a string which will replace any
'_' character in the tag."
  (let ((tagfile (ewo--tagfile (car tag))))
    (if (not (null tagfile))
	(let ((printtag (if rep-underscore (replace-regexp-in-string "_" rep-underscore (car tag)) (car tag))))
          (insert "- ")
	  (when (or ewo-tag-class ewo-tag-sizing)
	    (newline)
	    (org-cycle)
	    (insert "#+ATTR_HTML:")
	    (when ewo-tag-class
	      (insert " :class " ewo-tag-class))
	    (when ewo-tag-sizing
	      (insert
	       " :style font-size: "
	       (number-to-string
		(ewo--tag-size (length (cdr tag)) ewo--tag-max-ref ewo-tag-minsize ewo-tag-maxsize))
	       "em;"))
	    (newline)
	    (org-cycle))
	  (insert "[[file:tags/" tagfile "][" printtag "]] "
		  (if ewo-tag-show-numrefs (format "%d"(length (cdr tag))) ""))
          (newline)
          (ewo--gen-tagfile (concat "tags/" tagfile) tag))
      (message "skipping bad tag : %s" (car tag)))))
  

(defun ewo--tagfile-content ()
  "Generate the tags.org file at the root of the site. Generate
the files for each tag in the tags directory."
  (when ewo-tag-container-class
    (insert "#+ATTR_HTML: :class " ewo-tag-container-class)
    (newline))
  (ewo--compute-tag-max-ref)
  (let ((stack (avl-tree-stack ewo--tags)))
    (nlet loop ((tag (avl-tree-stack-pop stack)))
      (unless (null tag)
        (ewo--process-tag tag " ")
        (loop (avl-tree-stack-pop stack))))))


(defun ewo--clean-tag-files ()
  "Clean the directory containing tag files. Create it if it does
not exist."
  (let ((dir (concat ewo-root-dir "/tags")))
    (if (not (file-exists-p dir))
        (make-directory dir) 
      (let ((ls (directory-files dir t)))
        (dolist (f ls)
          (if (and (file-writable-p f) (file-regular-p f))
              (delete-file f)))))))

(defun ewo--clean-html-tag-files (publish-dir)
  "Clean the directory containing the html tag files. Do nothning
if it does not exist."
  (let ((dir (concat publish-dir "/tags")))
    (when (file-exists-p dir)
      (let ((ls (directory-files dir t)))
	(dolist (f ls)
	  (when (and (file-writable-p f) (file-regular-p f))
	    (delete-file f)))))))

(defun ewo--validate-tag (level tag &optional rep-underscore)
  "Verify that a TAG is a valid tag. LEVEL is the level of the category.

If TAG is valid, generate a string containing and html anchor
linking TAG to the tag file.

If specified, REP-UNDERSCORE is a string which will replace any
'_' character in the tag."
  (let ((tagfile-link (concat (ewo-rootlink level) "tags/" (ewo--tagfile tag ".html")))
        (tagfile-test (concat
                       (file-name-as-directory ewo-root-dir)
                       "tags/"
                       (ewo--tagfile tag ".org"))))
    (when (file-exists-p tagfile-test)
      (let ((printtag (if rep-underscore (replace-regexp-in-string "_" rep-underscore tag) tag)))
	(concat "<a href=\"" tagfile-link "\">"
		printtag
		"</a>")))))
    

(defun ewo-filetags (channel catname &optional sep)
  "Gets the filetags of the current file. Filter bad tags,
return the list as a string using separator SEP, or a space if
nil. CHANNEL is the communication channel, CATNAME is the name of
the category where the file resides. Each tag is hyperlinked to
its tag page.

This function is callable via the <lisp></lisp> mechanism."
  (if (or (null catname)
          (not (eq (plist-get (cdr (assoc-string catname ewo-categories)) :type) 'blog)))
      ""
    (let ((tagl (plist-get channel :filetags))
          (catlevel (ewo-get-level (plist-get channel :input-file))))
      (nlet loop ((l tagl))
        (cond
         ((null l) "")
         ((string= (car l) "") (loop (cdr l)))
         (t (concat
             (ewo--validate-tag catlevel (car l) " ")
             (when (not (null (cdr l)))
               (if sep sep " "))
             (loop (cdr  l)))))))))

(provide 'ewo-tags)
