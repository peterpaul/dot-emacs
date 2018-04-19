;; macros can be loaded just like interactive functions using 'M-x <name>'
(fset 'make-mark-marq
      (lambda (&optional arg)
	"Search and replace next occurrence of 'mark' with 'marq'."
	(interactive "p")
	(kmacro-exec-ring-item
	 (quote ([19 109 97 114 107 return backspace 113] 0 "%d"))
	 arg)))

(fset 'beautify-xml
      (lambda (&optional arg)
	"Keyboard macro."
	(interactive "p")
	(kmacro-exec-ring-item
	 (quote ([134217790 return 67108896 left 134217847 134217788 134217765 62 60 return 62 25 60 return 33 134217790 67108896 134217788 134217756] 0 "%d"))
	 arg)))

(provide 'macros)
