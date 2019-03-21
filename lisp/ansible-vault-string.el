;;; -*- lexical-binding: t; -*-

(defgroup ansible-vault-string nil
  "Customizations for encrypted ansible variables."
  :group 'data)

(defcustom ansible-vault-string-password-provider #'ansible-vault-string--password-prompt
  "Function or lambda used to get the vault password.
This customization variable can be used to integrate
`ansible-vault-string' with password managers."
  :type '(function)
  :group 'ansible-vault-string)

(defun ansible-vault-string--password-prompt ()
  "Prompt user to enter vault password."
  (read-passwd "Vault password: "))

(defun ansible-vault-string--find-file-up (file-name &optional directory)
  "Search for FILE-NAME in DIRECTORY and parent directories.
Returns the first match found, i.e. the one closest to DIRECTORY, or nil."
  (let ((cwd (or directory
                 default-directory)))
    (while (and (not (file-exists-p (expand-file-name file-name cwd)))
                (not (f-root-p cwd)))
      (message "%s" (expand-file-name file-name cwd))
      (setq cwd (file-name-directory (directory-file-name cwd))))
    (when (file-exists-p (expand-file-name file-name cwd))
      (expand-file-name file-name cwd))))

(defun ansible-vault-string--password-from-file ()
  "Read password from password file."
  (let ((password-file (ansible-vault-string--find-file-up ".vault-password")))
    (if password-file
        (with-temp-buffer
          (insert-file-contents password-file)
          (buffer-string))
      (error (format "Vault password file '%s' not found" ".vault-password")))))

(defun ansible-vault-string--password-from-file-or-prompt ()
  "Read password from password file, if that fails, prompt the user."
  (condition-case nil
      (ansible-vault-string--password-from-file)
    (error (ansible-vault-string--password-prompt))))

(defun ansible-vault-string--command (vault-command vault-string vault-password)
  "Execute `ansible-vault' VAULT-COMMAND on file with VAULT-STRING, with VAULT-PASSWORD.
VAULT-COMMAND should be either of `encrypt' or `decrypt'.
Returns the result as string."
  (let ((vault-var-file "/tmp/ansible-vault.el~var")
        (vault-passwd-file "/tmp/ansible-vault.el~passwd"))
    (unwind-protect
        (progn
          (with-temp-file vault-var-file
            (dolist (line (s-split "\n" vault-string))
              (insert (s-trim line))
              (insert "\n")))
          (with-temp-file vault-passwd-file
            (insert vault-password))
          (shell-command (format "ansible-vault \"%s\" --vault-password-file \"%s\" \"%s\""
                                 vault-command
                                 vault-passwd-file
                                 vault-var-file))
          (with-temp-buffer
            (insert-file-contents vault-var-file)
            (buffer-string)))
      (when (file-exists-p vault-passwd-file)
        (delete-file vault-passwd-file))
      (when (file-exists-p vault-var-file)
        (delete-file vault-var-file)))))

(defun ansible-vault-string-decrypt-region (beg end vault-password)
  "Decrypt the region using `ansible-vault`."
  (interactive
   (list (region-beginning)
         (region-end)
         (apply ansible-vault-string-password-provider nil)))
  (let* ((string-to-decrypt (buffer-substring beg end))
         (decrypted-string (ansible-vault-string--command "decrypt" string-to-decrypt vault-password)))
    (kill-region beg end)
    (insert decrypted-string)))

(defun ansible-vault-string-encrypt-region (beg end vault-password)
  "Encrypt the region using `ansible-vault`."
  (interactive
   (list (region-beginning)
         (region-end)
         (apply ansible-vault-string-password-provider nil)))
  (let* ((string-to-encrypt (buffer-substring beg end))
         (encrypted-string (ansible-vault-string--command "encrypt" string-to-encrypt vault-password)))
    (kill-region beg end)
    (insert encrypted-string)))

(provide 'ansible-vault-string)
;;; ansible-vault ends here
