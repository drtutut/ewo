;; Copyright 2018 Éric Würbel

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
;;; This file contains functions devoted to text processing.


(defvar ewo:header-restriction
  '(link superscript subscript underline bold italic code verbatim strike-through latex-fragment radio-target target timestamp line-break entity footnote-reference))

(defun ewo:cut-excerpt (s limit)
  "Cut string S to length LIMIT."
  (let ((tree (org-element-parse-secondary-string s ewo:header-restriction)))
    (ewo:extract-text-tree tree limit)))

(defun ewo:extract-text-tree (tree limit)
  "extract a text of maximum length LIMIT from parse tree TREE."
  (let* ((lg         0) ; hard to avoid side effect on this...
	 (addlg      (lambda (n) (setq lg (+ lg n)) nil))
	 (post-blank (lambda (pl)
		       (let ((n (plist-get pl :post-blank)))
			 (message "post-blank: (%d)" n)
			 (funcall addlg n)
			 (make-string n ?\s))))) 
    (nlet loop ((tr tree))
      (if (null tr)
	  ""
	(let ((elt (car tr)))
	  (cond ((stringp elt)
		 (if (>= lg limit)
					; possible because of post blanks
		     ""
		   (if (>= (+ lg (length elt)) limit)
		       (let ((extr (- limit lg)))
			 (funcall addlg (- limit lg))
			 (substring-no-properties elt 0 extr))
		     (funcall addlg (length elt))
		     (concat (substring-no-properties elt) (loop (cdr tr))))))
		((listp elt)
		 (let ((kwd (car elt)))
		   (cond ((eq kwd 'bold)
			  (concat "*" (loop (cddr elt)) "*"
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 ((eq kwd 'italic)
			  (concat "/" (loop (cddr elt)) "/"
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 ((eq kwd 'superscript)
			  (concat "^" (loop (cddr elt))
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 ((eq kwd 'subscript)
			  (concat "_" (loop (cddr elt))
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 ((eq kwd 'underline)
			  (concat "_" (loop (cddr elt)) "_"
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 ((eq kwd 'code)
			  (concat "~" (loop (cddr elt)) "~"
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 ((eq kwd 'verbatim)
			  (concat "=" (loop (cddr elt)) "="
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 ((eq kwd 'strike-through)
			  (concat "+" (loop (cddr elt)) "+"
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 ((eq kwd 'latex-fragment)
			  (concat (plist-get (cadr elt) :value)
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 ((eq kwd 'radio-target)
			  (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'target)
			  (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'footnote-reference)
			  (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'link)
			  (concat "[[" (plist-get (cadr elt) :raw-link) "][" (loop (cddr elt)) "]]"
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 ((eq kwd 'entity)
			  (funcall addlg (length (plist-get (cadr elt) :utf-8)))
			  (concat "\\" (plist-get (cadr elt) :name)
				  (when (plist-get (cadr elt) :use-brackets-p) "{}")
				  (when (< lg limit) (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))))
			 (t (loop (cdr tr))))))))))))

(provide 'ewo-test)
