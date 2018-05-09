;;; -*- lexical-binding: t; -*-
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

(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)

(setq use-package-verbose t)
(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

;; Checks for package update
(use-package auto-package-update
  :config
  (auto-package-update-maybe)
  (setq auto-package-update-interval 1)
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions t))

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    )

(use-package pretty-mode
  :config
  (global-pretty-mode t)
  (global-prettify-symbols-mode 1)

  ;; (pretty-deactivate-groups
  ;;  '(:equality :ordering :ordering-double :ordering-triple
  ;; 	       :arrows :arrows-twoheaded :punctuation
  ;; 	       :logic :sets))

  (pretty-activate-groups
   '(:sub-and-superscripts
     :greek
     :arithmetic-nary
     :equality
     :ordering
     :ordering-double
     :ordering-triple
     :arrows
     :arrows-twoheaded
     :punctuation
     :logic
     :sets
     ))
  )

;; Enable all-the-icons
(when (display-graphic-p)
  ;; NOTE must run `M-x all-the-icons-install-fonts`
  (use-package all-the-icons)
  (use-package all-the-icons-dired)
  )

;; Use custom theme

(use-package nord-theme
  :config
  (load-theme 'nord t)
  )

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
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
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

(use-package customize-modeline
  :load-path "lisp"
  )

;; Minimap
(when (display-graphic-p)
  (use-package minimap
    :quelpa
    ((minimap :fetcher github :repo "dengste/minimap") :upgrade t)
    :config
    (require 'minimap)
    (global-set-key [f9] 'minimap-mode)
    :init
    (setq minimap-window-location 'right)
    :custom-face
    (minimap-active-region-background ((t (:background "#4C566A"))))
    (minimap-current-line-face ((t (:background "#88C0D0" :foreground "#2E3440"))))
    )
  )

(use-package neotree
  :config
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-window-fixed-size nil)
  )

(use-package dashboard
  :config
  (require 'dashboard)
  (dashboard-setup-startup-hook)
  )

(use-package lastpass)

;; Org
(use-package org
  :config
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)
  )

;; Code completion
(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Additional completion packages
(use-package company-ansible)
(use-package company-lsp)
(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
  )
(use-package company-web)

(use-package magit)

(use-package monky)

(use-package flycheck)
;;(use-package flycheck-rust)

(use-package counsel)
(use-package counsel-tramp
  :after counsel
  )

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  )

(when (display-graphic-p)
  (use-package all-the-icons-ivy)
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
  :config
  (global-set-key (kbd "C-h C-m") 'discover-my-major)
  (global-set-key (kbd "C-h M-m") 'discover-my-mode)
  )

(use-package discover)

(use-package which-key
  :config
  (require 'which-key)
  (which-key-mode)
  )

(use-package helpful
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command)
  )

;; Nice package to automatically disassemble java .class files
(use-package autodisass-java-bytecode)

(use-package lsp-mode
  :config
  (require 'lsp-mode)
  )

(use-package lsp-ui
  :config
  (require 'lsp-ui)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

(use-package lsp-rust
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
    (require 'lsp-rust))
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  )
(use-package cargo)

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
(use-package yaml-mode)

(use-package vagrant)

(use-package restart-emacs)

(use-package sunrise-commander
  :quelpa
  ((sunrise-commander
    :fetcher github
    :repo "escherdragon/sunrise-commander")
   :upgrade t)
  :config
  (when (display-graphic-p)
    (require 'sunrise-x-buttons)
    (require 'sunrise-x-modeline)
    )
  )

(use-package diredfl
  :config
  (diredfl-global-mode)
  )

(use-package x509-mode)

(use-package jdee
  :config
  (setq jdee-server-dir "~/.emacs.d/jdee-server")
  )

(use-package 2048-game)

(when (display-graphic-p)
  (if (y-or-n-p-with-timeout "Start EXWM?" 3 nil)
      (use-package exwm
	:config
	(require 'exwm)
	(require 'exwm-config)
	(exwm-config-default)
	)
    )
  )

;; (use-package docker
;;   :ensure t
;;   )

(use-package symon
  :config
  (symon-mode)
  )

(use-package customize-eshell
  :load-path "lisp"
  )

(use-package popwin
  :config
  (popwin-mode 1)
  )

(use-package shell-pop
  :config
  (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
  (setq shell-pop-universal-key "C-c t")
  )

(use-package customize-move-lines
  :load-path "lisp"
  )

(use-package macros
  :load-path "lisp"
  )

(use-package x509-certificate
  :load-path "lisp"
  )

;; Start server if not running
(load "server")
(unless (server-running-p) (server-start))

(provide 'init)
;;; init ends here
