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
;;; This file contains functions devoted to blog articles management.
(require 'uuidgen)
(require 'ewo-util)

(defun ewo:article-compare (art1 art2)
  "Compares 2 items ART1 and ART2 in the blog articles list. The
comparison criteria is the date."
  (let ((date1 (plist-get (cdr art1) :date))
        (date2 (plist-get (cdr art2) :date)))
    (not (time-less-p (date-to-time date1) (date-to-time date2)))))
;;    (< (ewo:compare-standard-date date1 date2) 0)))


(defun ewo:last-articles (arts)
  "Returns the `ewo-last-articles' articles from ARTS."
  (let ((sorted (sort arts ewo:article_compare)))
    (nlet loop ((l sorted)
                (n ewo-last-articles))
      (if (or (null l) (= 0 n))
          '()
        (cons (car l) (loop (- n 1) (cdr l)))))))

(defun ewo:get-buffer-date ()
  "Gets the org keyword \"DATE\" value in the current buffer. If
it does not exist create it with the current date."
  (let ((d (ewo:read-org-option "DATE")))
    (if (null d)
        (save-excursion
          (goto-char (point-min))
          (end-of-line)
          (new-line)
          (insert "#+DATE: ")
          (org-time-stamp '(16))
          (new-line)
          (goto-char (point-min))
          (ewo:read-org-option "DATE"))
      d)))

(defun ewo:get-buffer-title ()
  "Gets the org keyword \"TITLE\" value in the current
buffer. Issue an error if it is not found."
  (let ((tit (ewo:read-org-option "TITLE")))
    (if (null tit)
        (user-error "No title in article")
      tit)))

(defun ewo:get-buffer-id ()
  "Gets the uid of the article in the current buffer. Create it
if it does not exist."
  (let ((uid (ewo:read-org-option "EWO_ARTICLE_ID")))
    (if (null uid)
        (save-excursion
          (goto-char (point-min))
          (end-of-line)
          (newline)
          (insert "#+EWO_ARTICLE_ID: ")
          (uuidgen nil)
          (goto-char (point-min))
          (ewo:read-org-option "EWO_ARTICLE_ID"))
      uid)))    

(defun ewo:get-buffer-excerpt ()
  "Read the ewo_head block, and return an excerpt of
`ewo-excerpt-size' long."
  (let* ((chapo (ewo:read-org-block "ewo_head"))
         (exc   (if chapo (substring chapo 0 (min ewo-excerpt-size (length chapo))) "")))
    (concat exc (if (string= exc "") "" "…"))))


(defun ewo:add-to-article-lists (id date title excerpt file)
  "Add a blog article to the article list of the category and to
the global articles list. If the article is already present,
issue an error.

ID is the unque identifier of the article, DATE is the date
present in the article, TITLE is the title of the article,
EXCERPT is an excerpt of the headlines of the article, and FILE
is the filename of the file containing the article."
  (when (assoc id ewo:blog-global-article-list)
    (user-error (format "article %s already in global table" id)))
  (when (assoc id ewo:blog-category-article-list)
    (user-error (format "article %s already in category table" id)))
  (setq ewo:blog-global-article-list
        (cons (list id :date date :title title :excerpt excerpt :file file)
              ewo:blog-global-article-list))
  (setq ewo:blog-category-article-list
        (cons (list id :date date :title title :excerpt excerpt :file file)
              ewo:blog-category-article-list)))

(defun ewo:category-indexp (file dir)
  "Check if FILE is the index of category rooted in DIR."
  (let ((fdir (file-name-directory file))
        (fname (file-name-nondirectory file)))
    (and (string= (file-name-as-directory dir) fdir)
         (string= fname "index.org"))))

(defun  ewo:create-minimal-cat-index (cat)
  "Creates a minimal category index in current buffer. CAT is the
category name."
  (insert (format "#+TITLE: %s / %s" ewo-name cat))
  (newline)
  (insert "#+DATE: " )
  (org-time-stamp '(16))
  (newline 2)
  (insert "* Published so far")
  (beginning-of-line)
  (org-entry-put (point) "HTML_CONTAINER_CLASS" "ewo-toc")
  (end-of-buffer)
  (newline)
  (save-buffer))


(defun ewo:prepare-cat-index-buffer (dir cat)
  "Prepare the category CAT index rooted in DIR.
Returns a pair (BUFFER . LEVEL) where BUFFER is the buffer of the
category index, and LEVEL is the heading level of the toc in
index."
  (let ((idxfile (concat (file-name-as-directory dir) "index.org")))
    (let* ((exist (file-exists-p idxfile))
           (visiting (find-buffer-visiting idxfile))
           (buf (or visiting (find-file-noselect idxfile))))
      (with-current-buffer buf
        (unless exist
          (ewo:create-minimal-cat-index cat))
        (goto-char (point-min))
        (cons buf
              (let ((sres (org-map-entries (lambda () (point)) "HTML_CONTAINER_CLASS={ewo-toc}}")))
                (if (null sres)
                    (progn
                                        ; create section
                      (end-of-buffer)
                      (newline 2)
                      (insert "* " ewo-blog-toc-name)
                      (beginning-of-line)
                      (org-entry-add-to-multivalued-property (point) "HTML_CONTAINER_CLASS" "ewo-toc")
                      (end-of-buffer)
                      (newline)
                      1)
                  (progn
                                        ; clean content, place cursor
                                        ; after properties
                    (goto-char (car sres))
                    (beginning-of-line)
                                        ; check level
                    (re-search-forward "\\(\\*+\\)\\s-" nil t)
                    (let ((level (length (match-string-no-properties 1 nil))))
                                        ; forward line exist because
                                        ; of the properties
                      (re-search-forward ":END:" nil t)
                      (end-of-line)
                                        ; clean existing entries
                      (let* ((begin (point))
                             (pos (re-search-forward (concat "^" (make-string level ?*) " ") nil t)))
                        (beginning-of-line)
                        (goto-char (if pos (point) (point-max)))
                        (delete-region begin (point))
                        (newline 2))
                      level)))))))))

(defun ewo:prepare-blog-index-buffer (dir)
  "Prepare the blog global index rooted in DIR.
Returns a pair (BUFFER . LEVEL) where BUFFER is the buffer of the
category index, and LEVEL is the heading level of the toc in
index."
  (let* ((idxfile (concat (file-name-as-directory dir) "index.org"))
         (visiting (find-buffer-visiting idxfile))
         (buf      (find-file-noselect idxfile)))
    (with-current-buffer buf
      (goto-char (point-min))
      (let ((sres (org-map-entries (lambda () (point)) "HTML_CONTAINER_CLASS=\{ewo-toc}")))
        (if (null sres)
            (progn
              (set-buffer-modified-p nil)
              (kill-buffer)
              nil)
          (progn
                                        ; clean content, place cursor
                                        ; after properties
            (goto-char (car sres))
            (beginning-of-line)
                                        ; check level
            (re-search-forward "\\(\\*+\\)\\s-" nil t)
            (let ((level (length (match-string-no-properties 1 nil))))
                                        ; forward line exist because
                                        ; of the properties
              (re-search-forward ":END:" nil t)
              (end-of-line)
                                        ; clean existing entries
              (let* ((begin (point))
                     (pos (re-search-forward (concat "^" (make-string level ?*) " ") nil t)))
                (beginning-of-line)
                (goto-char (if pos (point) (point-max)))
                (delete-region begin (point))
                (newline 2))
              (cons buf level))))))))

  
(defun ewo:make-toc-heading (art fmt)
  "Build a toc heading for article ART. Use the format specified
by FMT. Returns a string."
  (nlet loop ((idx 0))
    (if (string-match "\\([^%]*\\)%\\([^%]*\\)%" fmt idx)
        (let* ((mboiler-start  (nth 2 (match-data)))
               (mboiler-end    (nth 3 (match-data)))
               (mpercent-start (nth 4 (match-data)))
               (mpercent-end   (nth 5 (match-data)))
               (kwd            (substring fmt mpercent-start mpercent-end)))
          (concat
           (substring ewo-blog-toc-entry-format mboiler-start mboiler-end)
           (cond
            ((string= kwd "date")
             (format-time-string ewo-blog-toc-date-format
                                 (date-to-time (plist-get (cdr art) :date))))
            ((string= kwd "title")
             (plist-get (cdr art) :title))
            ((string= kwd "") "%")
            (t (user-error (format "bad format string ewo-blog-toc-entry-format: %s"
                                   ewo-blog-toc-entry-format))))
           (loop (+ 1 mpercent-end))))
      (substring ewo-blog-toc-entry-format idx))))
        

(defun ewo:blog-gen-cat-index (dir cat)
  "Generate category CAT index in directory DIR."
  (let* ((info  (ewo:prepare-cat-index-buffer dir cat))
         (buf   (car info))
         (level (cdr info)))
    (set-buffer buf)
    (setq ewo:blog-category-article-list (sort ewo:blog-category-article-list 'ewo:article-compare))
    (dolist (a ewo:blog-category-article-list)
                                        ; for each article
      (insert (format "%s [[file:%s][%s]]"
                      (make-string (+ 1 level) ?*)
                      (file-relative-name (plist-get (cdr a) :file) dir)
                      (ewo:make-toc-heading a ewo-blog-toc-entry-format)))
      (newline 2)
                                        ; add a link around excerpt ?
      (insert (plist-get (cdr a) :excerpt))
      (newline 2))
    (save-buffer)
    (kill-buffer)))


(defun ewo:gen-blog-index (dir)
  "Generate the global blog index in directory DIR."
  (let ((info (ewo:prepare-blog-index-buffer dir)))
    (unless (null info)
      (setq ewo:blog-global-article-list (sort ewo:blog-global-article-list 'ewo:article-compare))
      (let ((firsts (ewo:list-bslice ewo-last-articles ewo:blog-global-article-list))
            (buf    (car info))
            (level  (cdr info)))
        (set-buffer buf)
        (dolist (a ewo:blog-global-article-list)
          (insert (format "%s [[file:%s][%s]]"
                          (make-string (+ 1 level) ?*)
                          (file-relative-name (plist-get (cdr a) :file) dir)
                          (ewo:make-toc-heading a ewo-blog-toc-entry-format)))
          (newline 2)
                                        ; add a link around excerpt ?
          (insert (plist-get (cdr a) :excerpt))
          (newline 2))
        (save-buffer)))))

(defun ewo:cat-is-blog-p (catname)
  "Return t if category CATNAME is a blogging category, nil
otherwise."
  (let* ((catinfo (assoc-string catname ewo-categories)))
    (eq (plist-get (cdr catinfo) :type) 'blog)))
         



(provide 'ewo-blog)
