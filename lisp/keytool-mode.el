;;; -*- lexical-binding: t; -*-
(require 'origami)
(require 's)

(setq keytool-syntax-constants
      '(
        "Alias name"
        "AuthorityInfoAccess"
        "AuthorityKeyIdentifier"
        "BasicConstraints"
        "CRLDistributionPoints"
        "Certificate chain length"
        "Certificate fingerprint"
        "Certificate fingerprints"
        "CertificatePolicies"
        "CertificatePolicyId"
        "Certificate"
        "Creation date"
        "DistributionPoint"
        "Entry type"
        "ExtendedKeyUsages"
        "Extensions"
        "Issuer"
        "KeyIdentifier"
        "KeyUsage"
        "Keystore provider"
        "Keystore type"
        "ObjectId"
        "Owner"
        "PolicyQualifierInfo"
        "Serial number"
        "Signature algorithm name"
        "Subject Public Key Algorithm"
        "SubjectAlternativeName"
        "SubjectKeyIdentifier"
        "URIName"
        "Valid from"
        "Version"
        "accessLocation"
        "accessMethod"
        "qualifierID"
        "qualifier"
        "until"
        ))

(setq keytool-syntax-keywords
      '(
        "[0-9A-F][0-9A-F]\\(:[0-9A-F][0-9A-F]\\)+"
        "[0-9]+\\([\\.][0-9]+\\)+"
        "[0-9]+:\\( +[0-9A-F][0-9A-F]\\)+.*$"
        "caIssuers"
        "ocsp"
        "serverAuth"
        "clientAuth"
        "DigitalSignature"
        "Key_Encipherment"
        "DNSName"
        "trustedCertEntry"
        "PrivateKeyEntry"
        "MD5"
        "SHA1withRSA"
        "SHA1"
        "SHA256withRSA"
        "SHA256"
        "SHA384withRSA"
        "SHA384"
        ))

(defun keytool-reduce-list-of-strings-to-regex (list-of-strings)
  (seq-reduce (lambda (x y) (format "%s\\|%s" x y))
              (cdr list-of-strings)
              (car list-of-strings)))

(setq keytool-highlights
      (list (cons (keytool-reduce-list-of-strings-to-regex keytool-syntax-constants)
                  font-lock-constant-face)
            (cons (keytool-reduce-list-of-strings-to-regex keytool-syntax-keywords)
                  font-lock-keyword-face)))

(define-derived-mode keytool-mode special-mode "keytool"
  (define-key keytool-mode-map (kbd "<tab>") 'origami-recursively-toggle-node)
  (define-key keytool-mode-map (kbd "c") 'keytool-changealias)
  (define-key keytool-mode-map (kbd "d") 'keytool-delete)
  (define-key keytool-mode-map (kbd "g") 'keytool-list)
  (define-key keytool-mode-map (kbd "i") 'keytool-importcert)
  (define-key keytool-mode-map (kbd "q") 'kill-this-buffer)
  (define-key keytool-mode-map (kbd "r") 'keytool-list-rfc)
  (define-key keytool-mode-map (kbd "v") 'keytool-list-verbose)
  (setq font-lock-defaults '(keytool-highlights)))

(map-put origami-parser-alist 'keytool-mode (origami-markers-parser "-----BEGIN CERTIFICATE-----" "-----END CERTIFICATE-----"))

(defun keytool-init (file)
  "Initialize a new keytool buffer with a file."
  (interactive "fKeystore File: ")
  (message "Opening keystore: '%s'" file)
  (switch-to-buffer file)
  (keytool-mode)
  (make-local-variable 'keystore-filename)
  (setq keystore-filename file) 
  (make-local-variable 'keystore-passphrase)
  (setq keystore-passphrase nil)
  (origami-mode)
  (keytool-list))

(defun keytool-get-passphrase-with-prompt (prompt)
  (read-passwd (format "%s (%s): "
                       prompt
                       keystore-filename)))

(defun keytool-get-passphrase-lazy ()
  (when (not keystore-passphrase)
    (setq keystore-passphrase
          (keytool-get-passphrase-with-prompt "Keystore Passphrase")))
  keystore-passphrase)

(defun keytool-list-style (style)
  (when keystore-filename
    (let ((inhibit-read-only t)
          (keystore-password (keytool-get-passphrase-lazy)))
      (erase-buffer)
      (insert
       (shell-command-to-string
        (format "keytool -list -keystore '%s' -storepass '%s' %s"
                keystore-filename
                keystore-password
                style)))
      (goto-char (point-min))
      (while (re-search-forward "\n\n+" nil t)
        (replace-match "\n\n" nil nil))
      (goto-char (point-min))
      (origami-close-all-nodes (get-buffer keystore-filename)))))

(defun keytool-list ()
  (interactive)
  (keytool-list-style ""))

(defun keytool-list-verbose ()
  (interactive)
  (keytool-list-style "-v"))

(defun keytool-list-rfc ()
  (interactive)
  (keytool-list-style "-rfc"))

(defun keytool-importcert (cert-buffer cert-alias)
  "Import certificate from CERT-BUFFER with alias CERT-ALIAS."
  (interactive "bBuffer with certificate to import: \nsSet alias for certificate: ")
  (let ((keystore-file keystore-filename)
        (keystore-pass (keytool-get-passphrase-with-prompt (format "Enter password to import certificate from '%s'"
                                                                   cert-buffer))))
    (save-excursion
      (set-buffer cert-buffer)
      (shell-command-on-region (point-min)
                               (point-max)
                               (format "keytool -importcert -keystore '%s' -storepass '%s' -alias '%s' -noprompt"
                                       keystore-file
                                       keystore-pass
                                       cert-alias)))
    (keytool-list)))

(defun keytool--parse-alias-from-line-at-pos (pos)
  "Try to parse an aliase from the line at POS.

This function changes the position of the point, so wrap calls to this in `save-excursion'"
  (let* ((beg (progn (beginning-of-line) (point)))
         (end (progn (end-of-line) (point)))
         (line (buffer-substring-no-properties beg end)))
    (if (s-starts-with? "Alias name: " line)
        (s-replace-regexp "Alias name: " "" line)
      (if (s-contains? "," line)
          (substring line 0 (s-index-of "," line))
        nil))))

(defun keytool-delete (pos)
  "Delete the keystore entry at point POS."
  (interactive "d")
  (save-excursion
    (let* ((alias (or (keytool--parse-alias-from-line-at-pos pos)
                     (error "Current line does not contain an alias")))
           (keystore-pass (keytool-get-passphrase-with-prompt (format "Enter password to delete certificate '%s'"
                                                                      alias))))
      (shell-command (format "keytool -delete -keystore '%s' -storepass '%s' -alias '%s'"
                             keystore-filename
                             keystore-pass
                             alias))
      (keytool-list))))

(defun keytool-changealias (pos destalias)
  "Move an existing keystore entry from the line at POS to DESTALIAS."
  (interactive "d\nsDestination alias: ")
  (save-excursion
    (let* ((alias (or (keytool--parse-alias-from-line-at-pos pos)
                     (error "Current line does not contain an alias")))
           (keystore-pass (keytool-get-passphrase-with-prompt (format "Enter password to change alias '%s' to '%s'"
                                                                      alias
                                                                      destalias))))
      (shell-command (format "keytool -changealias -keystore '%s' -storepass '%s' -alias '%s' -destalias '%s'"
                             keystore-filename
                             keystore-pass
                             alias
                             destalias))
      (keytool-list))))

(provide 'keytool-mode)
