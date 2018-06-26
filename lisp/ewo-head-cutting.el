
(defvar ewo:header-restriction
  '(link superscript subscript underline bold italic code verbatim strike-through latex-fragment radio-target target timestamp line-break entity footnote-reference))

(defvar ewo:test-str1 "Voici un exemple de *paragraphe* qu'on va tenter de [[https://fr.wikipedia.org/wiki/Massacre_%C3%A0_la_tron%C3%A7onneuse][tronçonner] intelligemment\dots

/eh oui !/")

(defun ewo:cut-excerpt (s limit)
  "Cut string S to length LIMIT."
  (let ((tree (org-element-parse-secondary-string s ewo:header-restriction)))
    (nlet loop (pos 0)
      (if (or (= pos (length s)) (> pos limit)
          ""
        (let ((plist (text-properties-at pos s))
              (next-change
               (or (next-property-change pos s)
                   (length s))))
          ;; todo: process
          (concat (ewo:process-property s plist pos next-change limit)
                  (loop next-change)))))))

;; TODO : text properties in an org-parsed string are not quite the
;; same as the text properties in emacs. See in org source how to
;; process them.
(defun ewo:process-property (s plist beg end limit)
  "process property plist on string s between positions beg and end, not going beyond l"
