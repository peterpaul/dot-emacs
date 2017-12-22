(require 'package) ;; You might already have this line

(setq package-enable-at-startup nil)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives '("sc" . "http://joseito.republika.pl/sunrise-commander/"))

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
 '(default ((t (:family "Source Code Variable"))))
 '(minimap-active-region-background ((t (:background "DodgerBlue4")))))

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
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring))

;; Minimap
(use-package minimap
  :config
  (require 'minimap)
  (global-set-key [f9] 'minimap-mode)
  :init
  (setq minimap-window-location 'right)
  )

(use-package sublimity
  :config
  (require 'sublimity)
  (require 'sublimity-scroll)
;;  (require 'sublimity-map)
;;  (setq sublimity-map-size 20)
;;  (setq sublimity-map-fraction 0.3)
;;  (setq sublimity-map-text-scale -7)
;;  (require 'sublimity-attractive)
  (sublimity-mode 1)
;;  (sublimity-map-set-delay 0)
  )

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

(use-package discover)

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

(use-package dashboard
  :config
  (require 'dashboard)
  (dashboard-setup-startup-hook)
  )

(use-package multiple-cursors
  :config
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

(use-package ag)
(use-package ripgrep)
(use-package rg)
(use-package projectile)
(use-package projectile-ripgrep)
(use-package term-projectile)  

(use-package ansible)
(use-package ansible-doc)
(use-package ansible-vault)

(use-package vagrant)

(use-package sunrise-commander)
(use-package sunrise-x-buttons)
(use-package sunrise-x-modeline)

(use-package jdee
  :config
  (setq jdee-server-dir "~/.emacs.d/jdee-server")
  )

;; Use custom theme
;;(use-package dracula-theme
;;  :config (load-theme 'dracula t))
(use-package doom-themes
  :config
  (require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(use-package solaire-mode
  :config
  (require 'solaire-mode)

  ;; brighten buffers (that represent real files)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; itself off every time Emacs reverts the file
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)

  ;; highlight the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

  ;; if the bright and dark background colors are the wrong way around, use this
  ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
  ;; This should be used *after* you load the active theme!
  ;;
  ;; NOTE: This is necessary for themes in the doom-themes package!
  (solaire-mode-swap-bg)
  )

;; NOTE must run `M-x all-the-icons-install-fonts`
(use-package all-the-icons)
(use-package all-the-icons-dired)

(use-package spaceline)
(use-package spaceline-config
  :ensure
  spaceline
  :config
  (spaceline-helm-mode 1)
  (spaceline-spacemacs-theme))

;; Start server if not running
(load "server")
(unless (server-running-p) (server-start))
