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
;;; This file contains utility functions.
(require 'cl-lib)

;; named let (fron scheme !) is so cool.
(defmacro nlet (name bindings &rest body)
  "The so cool \"named let\" construct borrowed from scheme. It
  avoids a lot of horrible while loops with side effects."
  (declare (indent 2))
  `(cl-labels ((,name ,(mapcar 'car bindings)
                      ,@body))
     (,name ,@(mapcar 'cadr bindings))))


(defun ewo:fix-timestamp-string (date-string)
  "This is a piece of code copied from org-page.
Returns yyyy-mm-dd format of `DATE-STRING'.

For example:

   [Nov. 28, 1994]     => [1994-11-28]
   [November 28, 1994] => [1994-11-28]
   [11/28/1994]        => [1994-11-28]

Any \"day of week\", or \"time\" info, or any other parts of the string, are
discarded.
Code detail: URL `http://xahlee.org/emacs/elisp_parse_time.html'"
  (let ((date-str date-string)
        date-list year month date yyyy mm dd)
    (setq date-str (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" date-str))
    (cond
     ;; USA convention of mm/dd/yyyy
     ((string-match
       "^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$"
       date-str)
      (concat (match-string 3 date-str) "-" (match-string 1 date-str) "-"
              (match-string 2 date-str)))
     ((string-match
       "^\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$"
       date-str)
      (concat (match-string 3 date-str) "-" (match-string 1 date-str) "-"
              (match-string 2 date-str)))
     ;; some ISO 8601. yyyy-mm-dd
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$\
T[0-9][0-9]:[0-9][0-9]" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str) "-"
              (match-string 3 date-str)))
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$"
       date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str) "-"
              (match-string 3 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)$" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)$" date-str)
      (match-string 1 date-str))
     (t (progn
          (setq date-str
                (replace-regexp-in-string "January " "Jan. " date-str))
          (setq date-str
                (replace-regexp-in-string "February " "Feb. " date-str))
          (setq date-str
                (replace-regexp-in-string "March " "Mar. " date-str))
          (setq date-str
                (replace-regexp-in-string "April " "Apr. " date-str))
          (setq date-str
                (replace-regexp-in-string "May " "May. " date-str))
          (setq date-str
                (replace-regexp-in-string "June " "Jun. " date-str))
          (setq date-str
                (replace-regexp-in-string "July " "Jul. " date-str))
          (setq date-str
                (replace-regexp-in-string "August " "Aug. " date-str))
          (setq date-str
                (replace-regexp-in-string "September " "Sep. " date-str))
          (setq date-str
                (replace-regexp-in-string "October " "Oct. " date-str))
          (setq date-str
                (replace-regexp-in-string "November " "Nov. " date-str))
          (setq date-str
                (replace-regexp-in-string "December " "Dec. " date-str))
          (setq date-str
                (replace-regexp-in-string " 1st," " 1" date-str))
          (setq date-str
                (replace-regexp-in-string " 2nd," " 2" date-str))
          (setq date-str
                (replace-regexp-in-string " 3rd," " 3" date-str))
          (setq date-str
                (replace-regexp-in-string "\\([0-9]\\)th," "\\1" date-str))
          (setq date-str
                (replace-regexp-in-string " 1st " " 1 " date-str))
          (setq date-str
                (replace-regexp-in-string " 2nd " " 2 " date-str))
          (setq date-str
                (replace-regexp-in-string " 3rd " " 3 " date-str))
          (setq date-str
                (replace-regexp-in-string "\\([0-9]\\)th " "\\1 " date-str))
          (setq date-list (parse-time-string date-str))
          (setq year (nth 5 date-list))
          (setq month (nth 4 date-list))
          (setq date (nth 3 date-list))
          (setq yyyy (number-to-string year))
          (setq mm (if month (format "%02d" month) ""))
          (setq dd (if date (format "%02d" date) ""))
          (concat yyyy "-" mm "-" dd))))))

;; TODO should not be needed anymore. check
(defun ewo:compare-standard-date (date1 date2)
  "Compare two standard ISO 8601 format dates, format is as below:

2012-08-17

1. if date1 is earlier than date2, returns 1

2. if equal, returns 0

3. if date2 is earlier than date1, returns -1.

Code borrowed from org-page."
  (let* ((date-list1 (parse-time-string date1))
         (year1 (nth 5 date-list1))
         (month1 (nth 4 date-list1))
         (day1 (nth 3 date-list1))
         (date-list2 (parse-time-string date2))
         (year2 (nth 5 date-list2))
         (month2 (nth 4 date-list2))
         (day2 (nth 3 date-list2)))
    (cond ((< year1 year2) 1)
          ((> year1 year2) -1)
          (t (cond ((< month1 month2) 1)
                   ((> month1 month2) -1)
                   (t (cond ((< day1 day2) 1)
                            ((> day1 day2) -1)
                            (t 0))))))))



(defun ewo:read-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

(defun ewo:read-org-block (blname)
  "Read the data in a block in the current buffer. Return nil if
match is not found."
  (let* ((begin  (concat "^\\s-*#\\+[bB][eE][gG][iI][nN]_" blname "\\s-*"))
         (inside "\\(.*\\(?:\n.*\\)*?\\)")
         (end    (concat "\n\\s-*#\\+[eE][nN][dD]_" blname)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat begin inside end) nil t)
        (let ((match (match-string-no-properties 1 nil)))
          (ewo:remove-indent match))))))
                
(defun ewo:remove-indent (text)
  "Remove indentation in TEXT."
  (let ((out "")
        (start nil)
        (lines (split-string text "\n" nil "[[:blank:]]+")))
    (nlet loop ((l lines))
      (when (not (null l))
        (when (not (null (cdr l)))
          (setq out (concat out (car l) "\n")))
        (loop (cdr l))))
    out))
  
(defun ewo:list-bslice (num lst)
  "returns a list containing the NUM first elements of list
LST. If LST has fewer elements than NUM, a copy of LST is
returned."
  (if (or (<= num 0) (not (integerp num)))
      (user-error "number NUM should be a positive integer")
    (nlet loop ((l lst)
                (n num))
      (if (or (null l) (= n 0))
          '()
        (cons (car l) (loop (cdr l) (- n 1)))))))
          


(provide 'ewo-util)
