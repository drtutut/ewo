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
;;; This file contain tags processing functions

(require 'avl-tree)
(require 'ewo-util)

(defun ewo:tagtree-cmpfunc (elt1 elt2)
  (string< (car elt1) (car elt2)))

(defun ewo:tagtree-updatefunc (new match)
  (cons (car new) (cons (cadr new) (cdr match))))

(defun ewo:add-to-tag-map (tag title file)
  "a reference to file in the tag entry of the `ewo:tags' table."
  (avl-tree-enter ewo:tags (list tag (cons file title)) 'ewo:tagtree-updatefunc))

(defun ewo:tagfile-header (&optional tagname)
  "Generates the header of the tag file. If optionnal aggument
`TAGNAME' is provided, the function generates the index for this
tag, otherwise it consider that this is the global tags.org
file."
  (insert "#+TITLE: "
          (if tagname tagname "Tags"))
  (newline)
  (insert "#+DATE: ")
  (org-time-stamp '(16))
  (newline)
  (insert "#+DESCRIPTION: tag index for "
          (if tagname tagname (concat "site " ewo-name)))
  (newline 2))

(defun ewo:tagfile-tag-content (tag)
  "Generates the content of the tagfile for tag `TAG' in the
current buffer."
  (dolist (pair (cdr tag))
    (let ((file (car pair))
          (title (cdr pair)))
      (insert "- [[file:../" file "][" title "]]")
      (newline))))

(defun ewo:gen-tagfile (file tag)
  "Generate the tag FILE for TAG."
  (save-excursion
    (let ((buffer (find-file-noselect file)))
      (set-buffer buffer)
      (erase-buffer)
      (ewo:tagfile-header)
      (ewo:tagfile-tag-content tag)
      (save-buffer)
      (kill-buffer))))

(defun ewo:tagfile (tag)
  "computes the filename corresponding to a tag."
                                        ; simple solution (see later
                                        ; if something more robust is
                                        ; needed, but I don't thik so,
                                        ; because allowed chars in org
                                        ; tags are already
                                        ; [a-zA-Z0-9_@]
  (when (string-match "^[[:word:]0-9_@]+$" (car tag))
    (concat (car tag) ".org")))

(defun ewo:process-tag (tag)
  "Generates an entry in the index (current buffer). Generates
the file corresponding to the tag."
  (let ((tagfile (ewo:tagfile tag)))
    (if (not (null tagfile))
        (progn
          (insert "- [[file:tags/" tagfile "][" (car tag) "]] " (format "%d"(length (cdr tag))))
          (newline)
          (ewo:gen-tagfile (concat "tags/" tagfile) tag))
      (message "skipping bad tag : %s" (car tag)))))
  

(defun ewo:tagfile-content ()
  "Generate the tags.org file at the root of the site. Generate
the files for each tag in the tags directory."
  (let ((stack (avl-tree-stack ewo:tags)))
    (nlet loop ((tag (avl-tree-stack-pop stack)))
;;      (message "current tag : %s" tag) 
      (unless (null tag)
        (ewo:process-tag tag)
        (loop (avl-tree-stack-pop stack))))))


(defun ewo:clean-tag-files ()
  "Clean the directory containing tag files. Create it if it does
not exist."
  (let ((dir (concat ewo:current-root "/tags")))
    (if (not (file-exists-p dir))
        (make-directory dir) 
      (let ((ls (directory-files dir t)))
        (dolist (f ls)
          (if (and (file-writable-p f) (file-regular-p f))
              (delete-file f)))))))

(provide 'ewo-tags)
