;;; -*- lexical-binding: t; -*-
(require 'origami)

(define-derived-mode keytool-mode special-mode "keytool"
  (define-key keytool-mode-map (kbd "<tab>") 'origami-recursively-toggle-node)
  )

(map-put origami-parser-alist 'keytool-mode (origami-markers-parser "-----BEGIN CERTIFICATE-----" "-----END CERTIFICATE-----"))

(defun keytool-init (file)
  "Initialize a new keytool buffer with a file."
  (interactive "fKeystore File: ")
  (setq *keystore-filename* file)
  (setq *keystore-password* (read-passwd "Keystore Password: "))
  (message "Opening keystore: '%s'" file)
  (switch-to-buffer "keytool")
  (keytool-mode)
  (origami-mode)
  (keytool-render)
  (goto-char (point-min))
  (origami-close-all-nodes (get-buffer "keytool"))
  )

(defun keytool-render ()
  "Render keytool"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (shell-command-to-string (format "keytool -list -keystore '%s' -storepass '%s' -rfc"
                                             *keystore-filename*
                                             *keystore-password*)))
    ))


(provide 'keytool-mode)
