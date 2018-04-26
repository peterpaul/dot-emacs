;;; -*- lexical-binding: t; -*-

;; from http://fasciism.com/2017/01/24/x509-mode/

(defvar x509-certificate-mode-hook nil)

(defun x509-certificate-parse-command (encoding)
  "Parse the buffer using OpenSSL's x509 command and the specified encoding."
  (let ((errbuf (generate-new-buffer-name "*x509-parse-error*")))
    ;; Try to parse buffer using specified encoding.
    (shell-command-on-region
     (point-min)
     (point-max)
     (format "openssl x509 -text -noout -inform %s" encoding)
     (buffer-name)
     t
     errbuf)
    ;; Check for failure, represented by the existence of errbuf.
    (if (get-buffer errbuf)
        ;; Restore buffer to original state.
        (progn
          (kill-buffer errbuf)
          (insert x509-certificate-mode-text)
          nil)
      t)))

(defun x509-certificate-parse ()
  "Parse the buffer as a certificate, trying multiple encodings."
  (interactive)
  (if (not (eq x509-certificate-mode-display :raw))
      (error "The buffer is not in :raw mode, it's in %s mode."
             x509-certificate-mode-display)
    (let ((modified (buffer-modified-p)))
      ;; Save the contents of the buffer.
      (setq x509-certificate-mode-text (buffer-string))
      (read-only-mode -1)
      ;; Try to convert the buffer through different formats.
      (if (not (x509-certificate-parse-command "pem"))
          (if (not (x509-certificate-parse-command "der"))
              (error "Failed to parse buffer as X.509 certificate.")))
      (read-only-mode 1)
      ;; Restore previous modification state.
      (set-buffer-modified-p modified)
      (setq x509-certificate-mode-display :parsed))))

(defun x509-certificate-raw ()
  "Revert buffer to unparsed contents."
  (interactive)
  (if (not (eq x509-certificate-mode-display :parsed))
      (error "The buffer is not in :parsed mode, it's in %s mode."
             x509-certificate-mode-display)
    (let ((modified (buffer-modified-p)))
      ;; Delete the buffer, which currently contains the parsed format.
      (read-only-mode -1)
      (erase-buffer)
      ;; Convert the buffer into its raw format.
      (insert x509-certificate-mode-text)
      (read-only-mode 1)
      ;; Restore previous modification state.
      (set-buffer-modified-p modified)
      (setq x509-certificate-mode-display :raw))))

(defun x509-certificate-toggle-display ()
  "Toggle between raw and parsed displays of the buffer."
  (interactive)
  (cond ((eq x509-certificate-mode-display :parsed)
         (x509-certificate-raw))
        ((eq x509-certificate-mode-display :raw)
         (x509-certificate-parse))
        (t
         (error "Variable x509-certificate-mode-display is in an unknown state: %s"
                x509-certificate-mode-display))))

(defvar x509-certificate-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'x509-certificate-toggle-display)
    map)
  "Keymap for X.509 Certificate major mode")

;; (add-to-list 'auto-mode-alist '("\\.\\(der\\|crt\\|pem\\)$" . x509-certificate-mode))
;; (add-to-list 'magic-mode-alist '("-----BEGIN CERTIFICATE-----" . x509-certificate-mode))

(defun x509-certificate-mode ()
  "Major mode for viewing X.509 certificates"
  ;; Ensure this function is callable by M-x.
  (interactive)
  ;; Clear the slate.
  (kill-all-local-variables)
  ;; Use our key map just for this buffer
  (use-local-map x509-certificate-mode-map)
  ;; Set the symbol (computer-recognizable) and name (human-visible).
  (setq major-mode 'x509-certificate-mode
        mode-name "X.509")
  ;; Create the two buffer-local variables on which our functions depend.
  (defvar-local x509-certificate-mode-display :raw
    "Current display mode of the data")
  (defvar-local x509-certificate-mode-text nil
    "Original text of the buffer")
  ;; Run the customization hooks.
  (run-hooks 'x509-certificate-mode-hook)
  ;; Perform the initial parse of the buffer
  (x509-certificate-parse))

(defun x509--replace-all (search-regexp replacement)
  "Replace all occurrences of SEARCH-REGEXP with REPLACEMENT in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward search-regexp nil t)
      (replace-match replacement nil nil))))

(defun x509--prepare-buffer ()
  "Removes all leading and trailing spaces and empty lines from current buffer."
  (x509--replace-all  "^[[:blank:]]+" "")
  (x509--replace-all  "[[:blank:]]+$" "")
  (x509--replace-all  "
+" "
"))

(require 'x509-mode)

(add-hook 'x509-certificate-mode-hook (lambda () (x509-mode)))

(defun x509-view-region-as-x509-certificate (beg end)
  "Try to view the region as x509 certificate"
  (interactive
   (list (region-beginning) (region-end)))
  (save-excursion
    (let ((old-buffer (current-buffer))
	  (cert-buffer (get-buffer-create (format "*certificate from %s*" (buffer-name)))))
      (copy-to-buffer cert-buffer beg end)
      (with-current-buffer cert-buffer
	(barf-if-buffer-read-only)
	(goto-char (point-min))
	(insert "-----BEGIN CERTIFICATE-----\n")
	(goto-char (point-max))
	(insert "\n-----END CERTIFICATE-----\n")
	(x509--prepare-buffer)
	(deactivate-mark)
	(x509-certificate-mode)
	;; (x509-viewcert (format "x509 -text -noout -inform %s"
        ;;                                      (x509--buffer-encoding)))
	)
      (switch-to-buffer cert-buffer)
      ;;(kill-buffer cert-buffer)
      )))

(defun x509-view-xml-element-as-x509-certificate (pos)
  "Try to view the xml element under the point as x509 certificate"
  (interactive
   (list (point)))
  (let ((beg (or (save-excursion (when (search-backward ">" nil t)
				  (forward-char 1)
				  (point)))
		(point-min)))
	(end (or (save-excursion (when (search-forward "<" nil t)
				  (backward-char 1)
				  (point)))
		(point-max))))
    (x509-view-region-as-x509-certificate beg end)))

(provide 'x509-certificate)
