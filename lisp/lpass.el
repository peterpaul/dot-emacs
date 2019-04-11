;;; -*- lexical-binding: t; -*-
;;
;; Problems with emacs-lastpass.el
;; - no completable selection of accounts
;; - password is printed to message
;; - it is too easy to accidentally delete an account from the list-all view

(defcustom lpass-list--format
  (list '("ID" 20 nil)
        '("Group" 16 t)
        '("Name" 64 t)
        '("Username" 64 t))
  "Format for tabulated-list."
  :group 'lpass
  :type '(repeat (list (string :tag "Column name")
                       (integer :tag "Column width")
                       (choice (boolean :tag "Sort by value?")
                               (function :tag "Sorting predicate")))))

(defvar-local lpass-current-group nil
  "Current selected group.")

(defun lpass-status ()
  "Checks whether a user is logged in.
Return t when a user is logged in, nil otherwise."
  (interactive)
  (eq (shell-command "lpass status")
      0))

(defun lpass-login (&optional lpass-user)
  "Login LPASS-USER."
  (interactive (list (read-string "Lastpass user email: ")))
  (unless (lpass-status)
    (async-shell-command (format "lpass login %s" (shell-quote-argument lpass-user)))))

(defun lpass-logout ()
  "Logout."
  (interactive)
  (if (lpass-status)
      (when (y-or-n-p "Are you sure you want to logout?")
        (shell-command "lpass logout -f"))
    (message "lpass: You're not logged in")))

(defun lpass-list--entries (buffer)
  "Parse BUFFER and return a list of entries containing '(GROUP NAME ID)'.
BUFFER can be a buffer or a buffer name, and should contain the output of 'lpass ls'."
  (let (entries)
    (with-current-buffer buffer
      (goto-char (point-max))
      (while (re-search-backward "^\\([^~]*\\)~\\([^~]*\\)~\\([^~]*\\)~\\([^~]*\\)$" nil t)
        (setq entries (cons (list (match-string 1)
                                  (vector (match-string 1)
                                          (match-string 2)
                                          (match-string 3)
                                          (match-string 4)
                                          ))
                            entries))))
    entries))

(defun lpass-list-entries ()
  "List all accounts."
  (interactive)
  (unless (lpass-status)
    (error "Not logged in"))
  (let ((cmd (format "lpass ls --format=\"%s\" %s"
                     "%ai~%ag~%an~%au"
                     (shell-quote-argument (or lpass-current-group
                                               "")))))
    (with-temp-buffer
      (shell-command cmd
                     (current-buffer)
                     "*lpass errors*")
      (lpass-list--entries (current-buffer)))))

(define-derived-mode lpass-mode tabulated-list-mode "lpass"
  "\\<keystore-mode-map>
\\{keystore-mode-map}"
  ;; TODO before refresh of contents, reset tabulated-list-format
  (setq-local tabulated-list-format (vconcat lpass-list--format))
  (setq-local tabulated-list-padding 2)
  (setq-local tabulated-list-sort-key (cons "Group" nil))
  (setq-local tabulated-list-entries #'lpass-list-entries)
  (tabulated-list-init-header))

(defun lpass-list--column-value (column-index)
  "Return value column with COLUMN-INDEX for current line."
  (elt (tabulated-list-get-entry (point)) column-index))

(defun lpass-list--limit-group (pos)
  ""
  (interactive "d")
  (setq-local lpass-current-group (lpass-list--column-value 1))
  (tabulated-list-revert))

(defun lpass-list--reset-group ()
  ""
  (interactive)
  (setq-local lpass-current-group nil)
  (tabulated-list-revert))

(defun lpass-list ()
  ""
  (interactive)
  (with-current-buffer (get-buffer-create "*lpass list*")
    (lpass-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(defun lpass-show (pos)
  ""
  (interactive "d")
  (let ((id (tabulated-list-get-id pos)))
    (with-current-buffer (get-buffer-create "*lpass show*")
      (shell-command (format "lpass show %s" (shell-quote-argument id))
                     (current-buffer)
                     "*lpass errors*")
      (switch-to-buffer (current-buffer)))))

(defun lpass-copy-password (pos)
  ""
  (interactive "d")
  (let ((id (tabulated-list-get-id pos)))
    (with-temp-buffer
      (shell-command (format "lpass show --password %s" (shell-quote-argument id))
                     (current-buffer)
                     "*lpass errors*")
      (goto-char (point-min))
      (set-mark (point))
      (end-of-line)
      (kill-region (mark) (point)))))

(provide 'lpass)
;;; lpass.el ends here
