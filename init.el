(require 'package) ;; You might already have this line

(setq package-enable-at-startup nil)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize) ;; You might already have this line

(setq use-package-verbose t)
(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(delete-old-versions t)
 '(global-linum-mode t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-make-backup-files t)
 '(version-control t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro")))))

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

;; Checks for package update
(use-package auto-package-update
  :config
  (auto-package-update-maybe)
  (setq auto-package-update-delete-old-versions t))

;; Code completion
(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Additional completion packages
(use-package company-ansible)

(use-package magit)

;; Helm for minibuffer completion
(use-package helm
  :config (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring))

;; Minimap
(use-package minimap
  :config
  (require 'minimap)
  (global-set-key [f9] 'minimap-mode)
  )

(use-package sublimity
  :config
  (require 'sublimity)
  (require 'sublimity-scroll)
;  (require 'sublimity-map)
;  (setq sublimity-map-size 20)
;  (setq sublimity-map-fraction 0.3)
;  (setq sublimity-map-text-scale -7)
  ;(require 'sublimity-attractive)
  (sublimity-mode 1)
;  (sublimity-map-set-delay 0)
  )

;; Use custom theme
(use-package monokai-theme
  :config (load-theme 'monokai t))

;; Multi terminal emulation
(use-package multi-term
    :config (require 'multi-term))

(use-package markdown-mode)
(use-package markdown-preview-mode)
(use-package markdown-toc)
(use-package json-mode)

;; Bash completion setup
(use-package bash-completion
  :config (require 'bash-completion)
  (bash-completion-setup))

(use-package discover-my-major
  :config (global-set-key (kbd "C-h C-m") 'discover-my-major)
  (global-set-key (kbd "C-h M-m") 'discover-my-mode))

;; Nice package to automatically disassemble java .class files
(use-package autodisass-java-bytecode)

(use-package lsp-mode
  :config
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck))
  (require 'lsp-mode)
  )

(use-package lsp-rust
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
    (require 'lsp-rust))
  )

(use-package neotree
  :config
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle)
  )
