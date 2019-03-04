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

(use-package pretty-mode
  :config
  (progn
    ;; (global-pretty-mode t)
    ;; (global-prettify-symbols-mode 1)

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

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

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

(use-package beacon
  :config (beacon-mode 1))

(use-package highlight-defined
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

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

(use-package org-preview-html)

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
  :commands company-lsp
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

(use-package ido
  :config
  (progn
    (ido-mode 1)
    (setq ido-everywhere t)
    (setq ido-enable-flex-matching t)))

(use-package counsel
  :after ido)
(use-package counsel-tramp
  :after counsel)

(use-package ivy
  :after ido
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-count-format "%d/%d ")
    (global-set-key (kbd "C-S-s") 'swiper)
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

(use-package lsp-mode
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config (setq lsp-ui-sideline-enable t
                lsp-ui-sideline-show-symbol t
                lsp-ui-sideline-show-hover t
                lsp-ui-sideline-showcode-actions t
                lsp-ui-sideline-update-mode 'point))

;; First install rust language server with:
;;
;; $ rustup component add rls-preview rust-analysis rust-src
;; (use-package lsp-rust
;;   :if (command-exists-p "cargo")
;;   :config
;;   (progn
;;     (with-eval-after-load 'lsp-mode
;;       (setq lsp-rust-rls-command '("rls"))
;;       (require 'lsp-rust))
;;     (add-hook 'rust-mode-hook #'lsp-rust-enable)
;;     (add-hook 'rust-mode-hook #'flycheck-mode)
;;     ))

;; (use-package cargo
;;   :if (command-exists-p "cargo"))

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

(use-package iedit)

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

(use-package docker
  :if (command-exists-p "docker")
  :bind ("C-c d" . docker))

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

(use-package system-packages)

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

(use-package anzu)

(use-package exwm
  :if (display-graphic-p)
  :config
  (progn
    ;; Turn on `display-time-mode' if you don't use an external bar.
    (setq display-time-default-load-average nil)
    (display-time-mode t)

    ;; You are strongly encouraged to enable something like `ido-mode' to alter
    ;; the default behavior of 'C-x b', or you will take great pains to switch
    ;; to or back from a floating frame (remember 'C-x 5 o' if you refuse this
    ;; proposal however).
    ;; You may also want to call `exwm-config-ido' later (see below).
    (ido-mode 1)

;;;; Below are configurations for EXWM.

    ;; Add paths (not required if EXWM is installed from GNU ELPA).
                                        ;(add-to-list 'load-path "/path/to/xelb/")
                                        ;(add-to-list 'load-path "/path/to/exwm/")

    ;; Load EXWM.
    (require 'exwm)

    ;; Fix problems with Ido (if you use it).
    (require 'exwm-config)
    (exwm-config-ido)

    ;; Set the initial number of workspaces (they can also be created later).
    (setq exwm-workspace-number 4)

    ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
    ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
    ;; are run when a new X window class name or title is available.  Here's
    ;; some advice on this topic:
    ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
    ;; + For applications with multiple windows (e.g. GIMP), the class names of
                                        ;    all windows are probably the same.  Using window titles for them makes
    ;;   more sense.
    ;; In the following example, we use class names for all windows expect for
    ;; Java applications and GIMP.
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                            (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer exwm-class-name))))
    (add-hook 'exwm-update-title-hook
              (lambda ()
                (when (or (not exwm-instance-name)
                          (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer exwm-title))))

    ;; Global keybindings can be defined with `exwm-input-global-keys'.
    ;; Here are a few examples:
    (setq exwm-input-global-keys
          `(
            ;; Bind "s-r" to exit char-mode and fullscreen mode.
            ([?\s-r] . exwm-reset)
            ;; Bind "s-w" to switch workspace interactively.
            ([?\s-w] . exwm-workspace-switch)
            ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))
            ;; Bind "s-&" to launch applications ('M-&' also works if the output
            ;; buffer does not bother you).
            ([?\s-&] . (lambda (command)
		         (interactive (list (read-shell-command "$ ")))
		         (start-process-shell-command command nil command)))
            ;; Bind "s-<f2>" to "slock", a simple X display locker.
            ([s-f2] . (lambda ()
		        (interactive)
		        (start-process "" nil "/usr/bin/slock")))))

    ;; To add a key binding only available in line-mode, simply define it in
    ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

    ;; The following example demonstrates how to use simulation keys to mimic
    ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
    ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
    ;; and DEST is what EXWM actually sends to application.  Note that both SRC
    ;; and DEST should be key sequences (vector or string).
    (setq exwm-input-simulation-keys
          '(
            ;; movement
            ([?\C-b] . [left])
            ([?\M-b] . [C-left])
            ([?\C-f] . [right])
            ([?\M-f] . [C-right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end delete])
            ;; cut/paste.
            ([?\C-w] . [?\C-x])
            ([?\M-w] . [?\C-c])
            ([?\C-y] . [?\C-v])
            ;; search
            ([?\C-s] . [?\C-f])))

    ;; You can hide the minibuffer and echo area when they're not used, by
    ;; uncommenting the following line.
    ;; (setq exwm-workspace-minibuffer-position 'bottom)

    ;; RandR
    (require 'exwm-randr)
    ;; (setq exwm-randr-workspace-monitor-plist '(1 "HDMI-1" 2 "HDMI-2"))
    ;; (add-hook 'exwm-randr-screen-change-hook
    ;;           (lambda ()
    ;;             (start-process-shell-command
    ;;              "xrandr" nil "xrandr --output eDP-1 --off --output HDMI-2 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP-3 --off --output DP-2 --off --output DP-1 --off")))
    (exwm-randr-enable)

    ;; System tray
    (require 'exwm-systemtray)
    (exwm-systemtray-enable)
    )
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
