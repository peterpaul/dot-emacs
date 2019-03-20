;;; -*- lexical-binding: t; -*-

(defun ansible-vault-string--command (vault-command vault-string vault-password)
  "Execute `ansible-vault' VAULT-COMMAND on file with VAULT-STRING, with VAULT-PASSWORD.
VAULT-COMMAND should be either of `encrypt' or `decrypt'.
Returns the result as string."
  (let ((vault-var-file "/tmp/ansible-vault.el~var")
        (vault-passwd-file "/tmp/ansible-vault.el~passwd"))
    (unwind-protect
        (progn
          (with-temp-file vault-var-file
            (insert vault-string))
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
         (read-passwd "Vault password: ")))
  (let* ((string-to-decrypt (buffer-substring beg end))
         (decrypted-string (ansible-vault-string-command "decrypt" string-to-decrypt vault-password)))
    (kill-region beg end)
    (insert decrypted-string)))


(defun ansible-vault-string-encrypt-region (beg end vault-password)
  "Encrypt the region using `ansible-vault`."
  (interactive
   (list (region-beginning)
         (region-end)
         (read-passwd "Vault password: ")))
  (let* ((string-to-encrypt (buffer-substring beg end))
         (encrypted-string (ansible-vault-string-command "encrypt" string-to-encrypt vault-password)))
    (kill-region beg end)
    (insert encrypted-string)))

(provide 'ansible-vault-string)
;;; ansible-vault ends here
