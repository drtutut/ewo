

(defvar ewo:header-restriction
  '(link superscript subscript underline bold italic code verbatim strike-through latex-fragment radio-target target timestamp line-break entity footnote-reference))

(defvar ewo:test-str1 "Voici /un exemple^2 de *paragraphe*/ qu'on va tenter[fn:1] de [[https://fr.wikipedia.org/wiki/Massacre_%C3%A0_la_tron%C3%A7onneuse][tronçonner]] \\laquo\\nbsp{}intelligemment\\nbsp\\raquo\\dots

/eh oui !/ et même avec des cibles radio <<<radio>>> et des fragment latex $a+b$ etc.

Une date <2003-09-16 Tue> pour faire\\
bonne mesure.")

(defun ewo:cut-excerpt (s limit)
  "Cut string S to length LIMIT."
  (let ((tree (org-element-parse-secondary-string s ewo:header-restriction)))
    (ewo:extract-text-tree tree limit)))

;; TODO: le problème des "..." en trop.
(defun ewo:extract-text-tree (tree limit)
  "extract a text of maximum length LIMIT from parse tree TREE."
  (let* ((lg         0) ; hard to avoid side effect on this...
	 (addlg      (lambda (n) (setq lg (+ lg n)) nil))
	 (post-blank (lambda (pl)
		       (let ((n (plist-get pl :post-blank)))
			 (funcall addlg n)
			 (make-string n ?\s))))) 
    (nlet loop ((tr tree))
      (if (null tr)
	  ""
	(let ((elt (car tr)))
	  (cond ((stringp elt)
		 (if (> lg limit)
					; possible because of post blanks
		     "\\dots"
		   (if (>= (+ lg (length elt)) limit)
		       (progn
			 (concat (substring-no-properties elt 0 (- (+ lg (length elt)) limit)) "\\dots"))
		     (funcall addlg (length elt))
		     (concat (substring-no-properties elt) (loop (cdr tr))))))
		((listp elt)
		 (let ((kwd (car elt)))
		   (cond ((eq kwd 'bold)
			  (concat "*" (loop (cddr elt)) "*"
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'italic)
			  (concat "/" (loop (cddr elt)) "/"
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'superscript)
			  (concat "^" (loop (cddr elt))
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'subscript)
			  (concat "_" (loop (cddr elt))
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'underline)
			  (concat "_" (loop (cddr elt)) "_"
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'code)
			  (concat "~" (loop (cddr elt)) "~"
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'verbatim)
			  (concat "=" (loop (cddr elt)) "="
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'strike-through)
			  (concat "+" (loop (cddr elt)) "+"
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'latex-fragment)
			  (concat (plist-get (cadr elt) :value)
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'radio-target)
			  (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'target)
			  (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'footnote-reference)
			  (concat (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'link)
			  (concat "[[" (plist-get (cadr elt) :raw-link) "][" (loop (cddr elt)) "]]"
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 ((eq kwd 'entity)
			  (funcall addlg (length (plist-get (cadr elt) :utf-8)))
			  (concat "\\" (plist-get (cadr elt) :name)
				  (when (plist-get (cadr elt) :use-brackets-p) "{}")
				  (funcall post-blank (cadr elt)) (loop (cdr tr))))
			 (t (loop (cdr tr))))))))))))
