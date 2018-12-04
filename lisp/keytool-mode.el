;;; -*- lexical-binding: t; -*-
(require 'origami)

(define-derived-mode keytool-mode special-mode "keytool"
  (define-key keytool-mode-map (kbd "<tab>") 'origami-recursively-toggle-node)
  )

(map-put origami-parser-alist 'keytool-mode (origami-markers-parser "-----BEGIN CERTIFICATE-----" "-----END CERTIFICATE-----"))

(defun keytool-init (file)
  "Initialize a new keytool buffer with a file."
  (interactive "fKeystore File: ")
  (message "Opening keystore: '%s'" file)
  (switch-to-buffer file)
  (keytool-mode)
  (make-local-variable 'keystore-filename)
  (setq keystore-filename file)
  (origami-mode)
  (keytool-render))

(defun keytool-render ()
  "Render keytool"
  (interactive)
  (when keystore-filename
    (let ((inhibit-read-only t)
          (keystore-password (read-passwd "Keystore Password: ")))
      (erase-buffer)
      (insert
       (shell-command-to-string
        (format "keytool -list -keystore '%s' -storepass '%s' -rfc"
                keystore-filename
                keystore-password)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\n\n+" nil t)
          (replace-match "\n\n" nil nil)))
      (goto-char (point-min))
      (origami-close-all-nodes (get-buffer keystore-filename)))))

(provide 'keytool-mode)
