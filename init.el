;;; -*- lexical-binding: t; -*-
;; Upon startup, write message with startup details
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Temporarily change garbage collection settings, and disable file-name-handler
(let ((gc-cons-threshold-backup gc-cons-threshold)
      (gc-cons-percentage-backup gc-cons-percentage)
      file-name-handler-alist-backup file-name-handler-alist)
  ;; Change garbage collector settings and file-name-handler
  (setq gc-cons-threshold 402653184
	gc-cons-percentage 0.6
	file-name-handler-alist nil)
  ;; Then restore it as late as possible
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (message "Restored original settings")
	      (setq gc-cons-threshold gc-cons-threshold-backup
		    gc-cons-percentage gc-cons-percentage-backup
		    file-name-handler-alist (append file-name-handler-alist-backup file-name-handler-alist)))))

;; install straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun command-exists-p (command)
  "Checks whether COMMAND exists on this system.

The existence of COMMAND is checked using =which COMMAND=. So this function
will only work on systems where the command =which= exists."
  (let ((buf    (get-buffer-create "command-exists-buffer"))
        (retval nil))
    (setq retval (shell-command (format "which '%s'" command)))
    (kill-buffer buf)
    (eq retval 0)))

;; install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default 't)

(setq use-package-verbose t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    )

(use-package pretty-mode
  :config
  (progn (global-pretty-mode t)
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
	    ))))

;; Enable all-the-icons
;; NOTE must run `M-x all-the-icons-install-fonts`
(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package nord-theme)

;; Use custom theme
(use-package doom-themes
  :config
  (progn
    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (load-theme 'doom-nord t)
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme
    (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t "Enable bold universally")    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t "Enable italics universally") ; if nil, italics is universally disabled
  )

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(use-package solaire-mode
  :config
  (progn
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
    ))

(use-package guru-mode
  :hook prog-mode)

(use-package aggressive-indent
  :config
  (progn
    (global-aggressive-indent-mode 1)
    (add-to-list 'aggressive-indent-excluded-modes 'java-mode)))

(use-package feature-mode)

(use-package customize-modeline
  :straight nil
  :load-path "lisp"
  )

;; Minimap
(use-package minimap
  :if (display-graphic-p)
  :straight (minimap :type git :host github :repo "dengste/minimap")
  :config
  (global-set-key [f9] 'minimap-mode)
  :init
  (setq minimap-window-location 'right)
  :custom-face
  (minimap-active-region-background ((t (:background "#4C566A"))))
  (minimap-current-line-face ((t (:background "#88C0D0" :foreground "#2E3440")))))

(use-package neotree
  :config (global-set-key [f8] 'neotree-toggle))

(use-package dashboard
  :config
  (require 'dashboard)
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 10)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))))

(use-package lastpass
  :if (command-exists-p "lpass"))

;; Org
(use-package org
  :config
  (progn (global-set-key "\C-cl" 'org-store-link)
         (global-set-key "\C-ca" 'org-agenda)
         (global-set-key "\C-cc" 'org-capture)
         (global-set-key "\C-cb" 'org-iswitchb))
  )

;; large file support
(use-package vlf
  :config
  (require 'vlf-setup))

;; editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; yasnippet
(use-package yasnippet
  :config
  (progn
    (yas-load-directory "~/.emacs.d/snippets")
    (yas-global-mode 1)))

(use-package yasnippet-snippets
  :after (yasnippet))

;; Code completion
(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Additional completion packages
(use-package company-ansible
  :if (command-exists-p "ansible")
  :after company)

(use-package company-lsp
  :after (company lsp-mode)
  :init (add-to-list 'company-backends #'company-lsp)
  :config (setq company-lsp-enable-snippet t
                company-lsp-cache-candidates t))

(use-package company-quickhelp
  :after (company)
  :config (progn (company-quickhelp-mode 1)
                 (with-eval-after-load 'company
                   (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))))

(use-package company-web)

(use-package magit
  :if (command-exists-p "git"))

(use-package monky
  :if (command-exists-p "hg"))

(use-package flycheck
  :after (intero)
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save new-line))
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))

(use-package counsel)
(use-package counsel-tramp
  :after counsel)

(use-package ivy
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-count-format "%d/%d ")
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
    ))

(use-package all-the-icons-ivy
  :if (display-graphic-p)
  :config
  (all-the-icons-ivy-setup))

(use-package avy
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char2)
	 ("M-g f" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)
	 ("M-g e" . avy-goto-word-0)
	 ("C-c C-j" . avy-resume))
  :config
  (avy-setup-default)
  )

;; Multi terminal emulation
(use-package multi-term
  :bind (("<f5>" . 'multi-term)
         ("<C-next>" . 'multi-term-next)
         ("<C-prior>" . 'multi-term-prev))
  :custom
  (multi-term-buffer-name "term"))

(use-package xterm-color
  :config
  (progn
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))

    (add-hook 'shell-mode-hook
              (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

    ;; Also set TERM accordingly (xterm-256color)

    ;; You can also use it with eshell (and thus get color output from system ls):

    (require 'eshell)

    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))

    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

    ;;  Don't forget to setenv TERM xterm-256color
    ))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package js2-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)))

(use-package markdown-mode)
(use-package markdown-preview-mode)
(use-package markdown-toc)
(use-package json-mode)

;; Bash completion setup
(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package discover-my-major
  :config
  (progn
    (global-set-key (kbd "C-h C-m") 'discover-my-major)
    (global-set-key (kbd "C-h M-m") 'discover-my-mode)))

(use-package discover)

(use-package which-key
  :config
  (which-key-mode)
  )

(use-package helpful
  :config
  (progn
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
    (global-set-key (kbd "C-h C") #'helpful-command)))

;; Nice package to automatically disassemble java .class files
(use-package autodisass-java-bytecode)

(use-package logview)

(use-package lsp-mode)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config (setq lsp-ui-sideline-enable t
                lsp-ui-sideline-show-symbol t
                lsp-ui-sideline-show-hover t
                lsp-ui-sideline-showcode-actions t
                lsp-ui-sideline-update-mode 'point))

;; First install rust language server with:
;;
;; $ rustup component add rls-preview rust-analysis rust-src
(use-package lsp-rust
  :if (command-exists-p "cargo")
  :config
  (progn
    (with-eval-after-load 'lsp-mode
      (setq lsp-rust-rls-command '("rls"))
      (require 'lsp-rust))
    (add-hook 'rust-mode-hook #'lsp-rust-enable)
    (add-hook 'rust-mode-hook #'flycheck-mode)
    ))

(use-package cargo
  :if (command-exists-p "cargo"))

(use-package lsp-java
  :if (command-exists-p "javac")
  :defer 3
  :init
  (progn
    (require 'lsp-ui-flycheck)
    (require 'lsp-ui-sideline)
    (add-hook 'java-mode-hook #'lsp-java-enable)
    (add-hook 'java-mode-hook #'flycheck-mode)
    (add-hook 'java-mode-hook #'company-mode)
    (add-hook 'java-mode-hook (lambda () (lsp-ui-flycheck-enable t)))
    (add-hook 'java-mode-hook #'lsp-ui-sideline-mode)))

(use-package java-snippets
  :if (command-exists-p "javac")
  :after yasnippet
  :init (add-hook 'java-mode-hook #'yas-minor-mode))

(use-package haskell-mode
  :if (command-exists-p "stack"))

(use-package haskell-emacs
  :if (command-exists-p "stack"))

(use-package intero
  :if (command-exists-p "stack")
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package expand-region)

(use-package multiple-cursors
  :config
  (progn
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
    ))

(use-package ag
  :if (command-exists-p "ag"))
(use-package ripgrep
  :if (command-exists-p "rg"))
(use-package rg
  :if (command-exists-p "rg"))
(use-package projectile)
(use-package projectile-ripgrep
  :if (command-exists-p "rg"))
(use-package term-projectile)

(use-package ansible
  :if (command-exists-p "ansible")
  :config
  (progn
    (add-to-list 'auto-mode-alist '(".*inventory.*/group_vars/.*\\'" . yaml-mode))
    (add-to-list 'auto-mode-alist '(".*inventory.*/host_vars/.*\\'" . yaml-mode))
    ))

(use-package ansible-doc
  :if (command-exists-p "ansible"))
(use-package ansible-vault
  :if (command-exists-p "ansible"))
(use-package yaml-mode)

(use-package vagrant
  :if (command-exists-p "vagrant"))

(use-package restclient)

;; Reference Guide: http://plantuml.com/PlantUML_Language_Reference_Guide.pdf
;; Download from: https://sourceforge.net/projects/plantuml/files/plantuml.jar/download
(use-package plantuml-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
    (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
    ))

(use-package restart-emacs)

(use-package sunrise-commander
  :straight (sunrise-commander :type git :host github :repo "escherdragon/sunrise-commander")
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

(use-package visual-regexp)

(use-package package-lint)

(use-package x509-mode
  :if (command-exists-p "openssl"))

;; (use-package jdee
;;   :config
;;   (setq jdee-server-dir "~/.emacs.d/jdee-server")
;;   )

(use-package 2048-game)

(use-package exwm
  :if (display-graphic-p)
  )

;; (use-package docker
;;   )

;; (use-package symon
;;   :config
;;   (symon-mode)
;;   )

;; (use-package popwin
;;   :config
;;   (popwin-mode 1)
;;   )

(use-package shell-pop
  :config
  (progn
    (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
    (setq shell-pop-universal-key "C-c t"))
  )

(use-package origami)

;; (use-package customize-eshell
;;   :straight nil
;;   :load-path "lisp"
;;   )

(use-package customize-move-lines
  :straight nil
  :load-path "lisp"
  )

(use-package macros
  :straight nil
  :load-path "lisp"
  )

(use-package x509-certificate-region
  :if (command-exists-p "openssl")
  :straight
  (x509-certificate-region
   :type git
   :host github
   :repo "peterpaul/x509-certificate-region.el")
  :bind (("C-x x c" . x509-view-certificate)
         ("C-x x x" . x509-view-xml-element-as-x509-certificate)
         ("C-x x r" . x509-view-region-as-x509-certificate)
	 ("C-x x p" . x509-view-paragraph-as-x509-certificate)))

(use-package keystore-mode
  :if (command-exists-p "keytool")
  :straight
  (keystore-mode
   :type git
   :host github
   :repo "peterpaul/keystore-mode"))

;; Start server if not running
(load "server")
(unless (server-running-p) (server-start))

(provide 'init)
;;; init ends here
