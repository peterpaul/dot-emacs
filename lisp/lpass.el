;;; -*- lexical-binding: t; -*-
;;
;; Problems with emacs-lastpass.el
;; - no completable selection of accounts
;; - password is printed to message
;; - it is too easy to accidentally delete an account from the list-all view

(defcustom lpass-list--format
  (list '("ID" 20 nil)
        '("Group" 16 t)
        '("Name" 64 t))
  "Format for tabulated-list."
  :group 'lpass
  :type '(repeat (list (string :tag "Column name")
                       (integer :tag "Column width")
                       (choice (boolean :tag "Sort by value?")
                               (function :tag "Sorting predicate")))))

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
    (async-shell-command (format "lpass login %s" lpass-user))))

(defun lpass-logout ()
  "Logout."
  (interactive)
  (shell-command "lpass logout"))

(defun lpass-list--entries (buffer)
  "Parse BUFFER and return a list of entries containing '(GROUP NAME ID)'.
BUFFER can be a buffer or a buffer name, and should contain the output of 'lpass ls'."
  (let (entries)
    (with-current-buffer buffer
      (goto-char (point-max))
      (while (re-search-backward "^\\(?1:[^/]+\\)/\\(?2:[^\[]+\\)\\[id: \\(?3:[0-9]+\\)\\]$" nil t)
        (setq entries (cons (list (match-string 3)
                                  (vector (match-string 3)
                                          (match-string 1)
                                          (s-trim (match-string 2))
                                          ))
                            entries))))
    entries))

(defun lpass-list-entries ()
  "List all accounts."
  (interactive)
  (unless (lpass-status)
    (error "Not logged in"))
  (with-temp-buffer
    (shell-command "lpass ls"
                   (current-buffer)
                   "*lpass errors*")
    (lpass-list--entries (current-buffer))))

(define-derived-mode lpass-mode tabulated-list-mode "lpass"
  "\\<keystore-mode-map>
\\{keystore-mode-map}"
  ;; TODO before refresh of contents, reset tabulated-list-format
  (setq-local tabulated-list-format (vconcat lpass-list--format))
  (setq-local tabulated-list-padding 2)
  (setq-local tabulated-list-sort-key (cons "Group" nil))
  (setq-local tabulated-list-entries #'lpass-list-entries)
  (tabulated-list-init-header))

(defun lpass-list ()
  ""
  (interactive)
  (with-current-buffer (get-buffer-create "*lpass list*")
    (lpass-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'lpass)
;;; lpass.el ends here
