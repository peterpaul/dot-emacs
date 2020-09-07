;;; -*- lexical-binding: t; -*-

(defun comment-next-sexp (&optional ARG)
  "Comment the next sexp.
With ARG, comment the next ARG sexps.
Negative ARG means comment backwards across N sexps.
This command assumes point is not in a string or comment."
  (interactive)
  (let ((beg (point)))
    (forward-list ARG)
    (unless (eolp)
      (electric-newline-and-maybe-indent))
    (comment-region beg (point))
    (indent-for-tab-command)))

(provide 'my-lisp-mode)

