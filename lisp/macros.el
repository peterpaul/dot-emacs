;; macros can be loaded just like interactive functions using 'M-x <name>'
(fset 'make-mark-marq
      (lambda (&optional arg)
	"Search and replace next occurrence of 'mark' with 'marq'."
	(interactive "p")
	(kmacro-exec-ring-item
	 (quote ([19 109 97 114 107 return backspace 113] 0 "%d"))
	 arg)))

(provide 'macros)
