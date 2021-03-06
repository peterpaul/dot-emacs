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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; Customizations
(defgroup init nil
  "Customizations of the configurations in init.el."
  :group 'environment)

(defcustom my-init-pretty t
  "Enable pretty options in the configuration. Note that a change requires a restart of Emacs."
  :group 'init
  :type 'boolean)

(defcustom my-init-pretty-heavy nil
  "Enable pretty options that are heavy for older computers. Note that a change requires a restart of Emacs."
  :group 'init
  :type 'boolean)

(defcustom my-init-java nil
  "Enable java programming language support. Note that a change requires a restart of Emacs."
  :group 'init
  :type 'boolean)

(defcustom my-init-haskell nil
  "Enable haskell programming language support. Note that a change requires a restart of Emacs."
  :group 'init
  :type 'boolean)

(defcustom my-init-rust nil
  "Enable rust programming language support. Note that a change requires a restart of Emacs."
  :group 'init
  :type 'boolean)

(defcustom my-init-ansible nil
  "Enable ansible support. Note that a change requires a restart of Emacs."
  :group 'init
  :type 'boolean)

(defcustom my-init-javascript nil
  "Enable javascript programming language support. Note that a change requires a restart of Emacs."
  :group 'init
  :type 'boolean)

(defcustom my-init-exwm t
  "Enable exwm window manager support. Note that a change requires a restart of Emacs."
  :group 'init
  :type 'boolean)

(defcustom my-init-exwm-systemtray nil
  "Enable exwm systemtray. Note that a change requires a restart of Emacs."
  :group 'init
  :type 'boolean)

(defcustom my-init-use-straight nil
  "Control whether to use `straight' or `quelpa'."
  :group 'init
  :type 'boolean)

(if my-init-use-straight
    (defmacro my-init-straight-or-quelpa (straight-body quelpa-body)
      `(,@straight-body))
  (defmacro my-init-straight-or-quelpa (straight-body quelpa-body)
    `(,@quelpa-body)))

(my-init-straight-or-quelpa
 (progn
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
   ;; install use-package
   (straight-use-package 'use-package)
   (setq straight-use-package-by-default 't)

   (setq use-package-verbose t))
 (progn
   (setq package-enable-at-startup nil)

   (require 'package) ;; You might already have this line

   (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		       (not (gnutls-available-p))))
	  (proto (if no-ssl "http" "https")))
     (when no-ssl
       (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
     ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
     (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
     ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
     (when (< emacs-major-version 24)
       ;; For important compatibility libraries like cl-lib
       (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

   (package-initialize)

   (unless (package-installed-p 'quelpa)
     (package-refresh-contents)
     (package-install 'quelpa))
   (require 'quelpa)

   (setq quelpa-use-package-inhibit-loading-quelpa nil)

   (quelpa
    '(quelpa-use-package
      :fetcher git
      :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
   (eval-when-compile
     (require 'quelpa-use-package))

   (setq use-package-verbose t)
   (setq use-package-always-ensure t)

   (unless (package-installed-p 'use-package)
     (package-refresh-contents)
     (package-install 'use-package))

   (eval-when-compile
     (require 'use-package))

   (use-package auto-package-update
     :config (auto-package-update-maybe))))

(defun command-exists-p (command)
  "Checks whether COMMAND exists on this system.

The existence of COMMAND is checked using =which COMMAND=. So this function
will only work on systems where the command =which= exists."
  (let ((buf    (get-buffer-create "command-exists-buffer"))
        (retval nil))
    (setq retval (shell-command (format "which '%s'" command)))
    (kill-buffer buf)
    (eq retval 0)))

(add-hook 'prog-mode-hook #'linum-mode)
(put 'narrow-to-region 'disabled nil)

(when my-init-pretty
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
  (use-package all-the-icons-ivy
    :after (ivy)
    :if (display-graphic-p)
    :config
    (all-the-icons-ivy-setup))
  (my-init-straight-or-quelpa
   (eval-when-compile
     (use-package customize-modeline
       :straight nil
       :load-path "lisp"))
   (eval-when-compile
     (use-package customize-modeline
       :load-path "lisp")))
  (use-package org-bullets
    :after (org)
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package diff-hl
    :config
    (global-diff-hl-mode))
  (use-package solaire-mode
    :after (nord-theme doom-themes)
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
      (solaire-mode-swap-bg)))
  (use-package diredfl
    :config
    (diredfl-global-mode))
  (unless my-init-exwm
    (use-package dashboard
      :config
      (require 'dashboard)
      (dashboard-setup-startup-hook)
      (setq dashboard-items '((recents  . 10)
                              (bookmarks . 10)
                              (projects . 5)
                              (agenda . 5)
                              (registers . 5)))))

  ;; Minimap
  (use-package minimap
    :if (display-graphic-p)
    :config
    (global-set-key [f9] 'minimap-mode)
    :init
    (setq minimap-window-location 'right)
    :custom-face
    (minimap-active-region-background ((t (:background "#4C566A"))))
    (minimap-current-line-face ((t (:background "#88C0D0" :foreground "#2E3440"))))))

(when my-init-pretty-heavy
  (use-package aggressive-indent
    :config
    (progn
      (global-aggressive-indent-mode nil)
      (add-to-list 'aggressive-indent-excluded-modes 'java-mode)))
  (use-package beacon
    :config (beacon-mode 1))
  (use-package rainbow-mode)
  (use-package rainbow-delimiters
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
  (use-package highlight-defined
    :config
    (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))
  ;; (use-package sidebar
  ;;   :straight (sidebar
  ;;              :type git
  ;;              :host github
  ;;              :repo "sebastiencs/sidebar.el"))
  (use-package smartparens
    :config
    (require 'smartparens-config)))

;; (use-package nord-theme)

;; Use custom theme
(use-package doom-themes
  :after (treemacs)
  :config
  (progn
    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (load-theme 'doom-nord t)
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom treemacs theme
    (doom-themes-treemacs-config)  ; all-the-icons fonts must be installed!
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t "Enable bold universally")    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t "Enable italics universally") ; if nil, italics is universally disabled
  (doom-themes-treemacs-theme "doom-colors" "Use the colorful treemacs theme") ; use the colorful treemacs theme
  )

(use-package guru-mode
  :hook prog-mode)

(use-package feature-mode)

(use-package treemacs
  :config (global-set-key [f8] 'treemacs))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package lastpass
  :if (command-exists-p "lpass"))

(use-package string-inflection
  :config (progn
            (global-set-key (kbd "C-c i") 'string-inflection-cycle)
            (global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
            (global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
            (global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles
            ))

;;______________________________________________________________________
;;;;  Installing Org with straight.el
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#installing-org-with-straightel
(require 'subr-x)
(use-package git)

(defun org-git-version ()
  "The Git version of 'org-mode'.
Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of 'org-mode'.
Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(my-init-straight-or-quelpa
 (use-package org-plus-contrib
   :mode (("\\.org$" . org-mode))
   :bind
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ;; (global-set-key "C-c b" org-iswitchb)
   )
 (use-package org
   :mode (("\\.org$" . org-mode))
   :bind
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   )
 )

(use-package org-preview-html
  :after (org))

(use-package org-brain
  :after (org)
  :init
  (setq org-brain-path "~/Documents/brain")
  ;; For Evil users
  ;; (with-eval-after-load 'evil
  ;;   (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  ;; (push '("b" "Brain" plain (function org-brain-goto-end)
  ;;         "* %i%?" :empty-lines 1)
  ;;       org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))

(use-package htmlize)

(use-package gnuplot
  :config
  (progn
    ;; these lines enable the use of gnuplot mode
    (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
    (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

    ;; this line automatically causes all files with the .gp extension to be loaded into gnuplot mode
    (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

    ;; This line binds the function-9 key so that it opens a buffer into gnuplot mode
    ;; (global-set-key [(f9)] 'gnuplot-make-buffer)
    ))

(use-package epresent
  :after (org))

(use-package org-present
  :after (org)
  :config
  (eval-after-load "org-present"
    '(progn
       (add-hook 'org-present-mode-hook
                 (lambda ()
                   (org-present-big)
                   (org-display-inline-images)
                   (org-present-hide-cursor)
                   (org-present-read-only)))
       (add-hook 'org-present-mode-quit-hook
                 (lambda ()
                   (org-present-small)
                   (org-remove-inline-images)
                   (org-present-show-cursor)
                   (org-present-read-write))))))

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

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Additional completion packages
(use-package company-lsp
  :after (company lsp-mode)
  :commands company-lsp
  :init (add-to-list 'company-backends #'company-lsp)
  :config (setq company-lsp-enable-snippet t
                company-lsp-cache-candidates t))

(use-package company-quickhelp
  :after (company)
  :config (progn (company-quickhelp-mode 1)
                 (with-eval-after-load 'company-mode
                   (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))))

(use-package company-web)

(use-package magit
  :if (command-exists-p "git"))

(use-package forge
  :if (command-exists-p "git")
  :after (magit))

(use-package monky
  :if (command-exists-p "hg"))

(use-package flycheck
  :after (intero)
  :hook (prog-mode . flycheck-mode)
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

(use-package flycheck-package
  :after (flycheck))

(use-package counsel
  :after ido
  :config (counsel-mode 1))

(use-package counsel-tramp
  :after counsel)

(use-package ivy
  :after ido
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-count-format "%d of %d - ")
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
    ;; (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-rg)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)))

(use-package avy
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char2)
	 ("M-g f" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)
	 ("M-g e" . avy-goto-word-0)
	 ("C-c C-j" . avy-resume))
  :config
  (avy-setup-default))

;; Multi terminal emulation
(use-package multi-term
  :bind (("<f5>" . 'multi-term)
         ("<C-next>" . 'multi-term-next)
         ("<C-prior>" . 'multi-term-prev))
  :custom
  (multi-term-buffer-name "term"))

(use-package xterm-color
  :if nil
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

    (add-to-list 'eshell-preoutput-filter-functions #'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

    ;;  Don't forget to setenv TERM xterm-256color
    ))

(use-package ansi-color
  :config
  (progn
    (defun colorize-compilation-buffer ()
      (ansi-color-apply-on-region compilation-filter-start (point-max)))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm)

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; Bash completion setup
(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package markdown-mode)

(defun markdown-backward-inner-block ()
  "Navigate back to the beginning of this block."
  (markdown-backward-block)
  (forward-line))

(defun markdown-forward-inner-block ()
  "Navigate forward to the end of this block."
  (markdown-forward-block)
  (previous-line))

(defun markdown-narrow-inner-block ()
  "Make text inside current block visible.
The current block is the one that contains point or follows point."
  (interactive)
  (let ((beginning-of-defun-function 'markdown-backward-inner-block)
        (end-of-defun-function 'markdown-forward-inner-block))
    (narrow-to-defun)))

(use-package markdown-preview-mode)
(use-package markdown-toc)
(use-package json-mode)

(when my-init-javascript
  (use-package js2-mode
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)))

  (use-package typescript-mode)

  (use-package tide
    :after (typescript-mode company flycheck)
    :hook ((typescript-mode . tide-setup)
           (typescript-mode . tide-hl-identifier-mode)
           (before-save . tide-format-before-save)))

  (use-package ts-comint))

(use-package discover-my-major
  :config
  (progn
    (global-set-key (kbd "C-h C-m") 'discover-my-major)
    (global-set-key (kbd "C-h M-m") 'discover-my-mode)))

(use-package discover)

(use-package which-key
  :config
  (which-key-mode))

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

(use-package logview)

(use-package lsp-mode
  :hook ((rust-mode . lsp)
         (lsp-mode .  lsp-enable-which-key-integration))
  :commands lsp
  :config (progn
            (setq lsp-rust-server 'rust-analyzer)))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config (setq lsp-ui-sideline-enable t
                lsp-ui-sideline-show-symbol t
                lsp-ui-sideline-show-hover t
                lsp-ui-sideline-show-code-actions t
                lsp-ui-sideline-update-mode 'point))

(use-package lsp-ivy
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(when my-init-rust
  (use-package cargo
    :if (command-exists-p "cargo")
    :hook (rust-mode . cargo-minor-mode))

  (use-package flycheck-rust
    :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

  (use-package toml-mode))

(when my-init-java
  ;; Nice package to automatically disassemble java .class files
  (use-package autodisass-java-bytecode)

  (use-package lsp-java
    :if (command-exists-p "javac")
    :after (lsp-mode)
    :defer 3
    :init
    (progn
      (require 'lsp-ui-flycheck)
      (require 'lsp-ui-sideline)
      (add-hook 'java-mode-hook #'lsp)
      (add-hook 'java-mode-hook #'flycheck-mode)
      (add-hook 'java-mode-hook #'company-mode)
      (add-hook 'java-mode-hook (lambda () (lsp-ui-flycheck-enable t)))
      (add-hook 'java-mode-hook #'lsp-ui-sideline-mode)))

  (use-package java-snippets
    :if (command-exists-p "javac")
    :after yasnippet
    :init (add-hook 'java-mode-hook #'yas-minor-mode))

  ;; (use-package dap-java :after (lsp-java))
  ;; (use-package lsp-java-treemacs :after (treemacs))

  (use-package kotlin-mode))

(when my-init-haskell
  (use-package haskell-mode
    :if (command-exists-p "stack"))

  (use-package haskell-emacs
    :if (command-exists-p "stack"))

  (use-package intero
    :if (command-exists-p "stack")
    :config
    (add-hook 'haskell-mode-hook 'intero-mode)))

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

(use-package csv-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
    ))

(when my-init-ansible
  (use-package ansible
    :config
    (progn
      (add-to-list 'auto-mode-alist '(".*inventory.*/group_vars/.*\\'" . yaml-mode))
      (add-to-list 'auto-mode-alist '(".*inventory.*/host_vars/.*\\'" . yaml-mode))
      ))

  (use-package ansible-doc
    :after (ansible))
  (use-package ansible-vault
    :after (ansible))
  (use-package company-ansible
    :after (ansible company))
  (use-package yaml-mode)
  (my-init-straight-or-quelpa
   (eval-when-compile
     (use-package ansible-vault-string
       :straight (ansible-vault-string
		  :type git
		  :host github
		  :repo "peterpaul/ansible-vault-string")))
   (eval-when-compile
     (use-package ansible-vault-string
       :quelpa (ansible-vault-string
                :fetcher github
                :repo "peterpaul/ansible-vault-string")))))

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

(my-init-straight-or-quelpa
 (eval-when-compile
   (use-package sunrise-commander
     :straight (sunrise-commander :type git :host github :repo "escherdragon/sunrise-commander")
     :config
     (when (display-graphic-p)
       (require 'sunrise-x-buttons)
       (require 'sunrise-x-modeline))))
 (eval-when-compile
   (use-package sunrise-commander
     :quelpa (sunrise-commander :fetcher github :repo "escherdragon/sunrise-commander")
     :config
     (when (display-graphic-p)
       (require 'sunrise-x-buttons)
       (require 'sunrise-x-modeline)))))

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

(when my-init-exwm
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
      ;;   all windows are probably the same.  Using window titles for them makes
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
      (when my-init-exwm-systemtray
        (require 'exwm-systemtray)
        (exwm-systemtray-enable))))

  (defun peterpaul/screenlayout (layout)
    (interactive
     (list
      (completing-read "Select the screenlayout: "
                       (seq-filter (lambda (x) (s-ends-with? ".sh" x)) (directory-files "~/.screenlayout")))))
    (let ((command (format "~/.screenlayout/%s" layout)))
      (message "command: %s" command)
      (start-process-shell-command "bash" nil command)))

  (defun peterpaulk/xrandr-desk-3-window ()
    (interactive)
    (start-process-shell-command "xrandr" nil "--output eDP-1 --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-2 --mode 1920x1080 --pos 3840x0 --rotate normal --output HDMI-1 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output DP-3 --off --output DP-2 --off --output DP-1 --off")
    (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "HDMI-1" 2 "HDMI-2"))
    (exwm-randr-refresh))

  (defun peterpaulk/xrandr-desk-2-window ()
    (interactive)
    (start-process-shell-command "xrandr" nil "--output eDP-1 --off --output HDMI-2 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP-3 --off --output DP-2 --off --output DP-1 --off")
    (setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1" 1 "HDMI-2"))
    (exwm-randr-refresh))

  (defun peterpaulk/xrandr-desk-home ()
    (interactive)
    (start-process-shell-command "xrandr" nil "--output eDP-1 --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-1 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-2 --off --output DP-3 --off --output DP-2 --off --output DP-1 --off")
    (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "HDMI-1"))
    (exwm-randr-refresh))

  (defun peterpaulk/xrandr-laptop ()
    (interactive)
    (start-process-shell-command "xrandr" nil "--output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-2 --off --output HDMI-1 --off --output DP-3 --off --output DP-2 --off --output DP-1 --off")
    (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1"))
    (exwm-randr-refresh))

  (defun peterpaulk/xrandr-attic ()
    (interactive)
    (start-process-shell-command "xrandr" nil "--output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-2 --off --output HDMI-1 --off --output DP-3 --off --output DP-2 --off --output DP-1 --off")
    (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "DP-3"))
    (exwm-randr-refresh)))

(use-package popwin
  :config
  (popwin-mode 1))

(use-package shell-pop
  :config
  (progn
    (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
    (setq shell-pop-universal-key "C-c t")))

;; (use-package origami)

;; (use-package customize-eshell
;;   :load-path "lisp")

(my-init-straight-or-quelpa
 (eval-when-compile
   (use-package custom-commands
     :straight nil
     :load-path "lisp"))
 (eval-when-compile
   (use-package custom-commands
     :load-path "lisp")))

(my-init-straight-or-quelpa
 (eval-when-compile
   (use-package my-lisp-mode
     :straight nil
     :load-path "lisp"))
 (eval-when-compile
   (use-package my-lisp-mode
     :load-path "lisp")))

(my-init-straight-or-quelpa
 (eval-when-compile
   (use-package customize-move-lines
     :straight nil
     :load-path "lisp"))
 (eval-when-compile
   (use-package customize-move-lines
     :load-path "lisp")))

(my-init-straight-or-quelpa
 (eval-when-compile
   (use-package macros
     :straight nil
     :load-path "lisp"))
 (eval-when-compile
   (use-package macros
     :load-path "lisp")))

(my-init-straight-or-quelpa
 (eval-when-compile
   (use-package lpass
     :straight (lpass
		:type git
		:host github
		:repo "peterpaul/lpass")))
 (eval-when-compile
   (use-package lpass
     :quelpa (lpass
              :fetcher github
              :repo "peterpaul/lpass"))))

(my-init-straight-or-quelpa
 (eval-when-compile
   (use-package x509-certificate-region
     :if (command-exists-p "openssl")
     :straight (x509-certificate-region
		:type git
		:host github
		:repo "peterpaul/x509-certificate-region.el")
     :bind (("C-x x c" . x509-view-certificate)
            ("C-x x x" . x509-view-xml-element-as-x509-certificate)
            ("C-x x r" . x509-view-region-as-x509-certificate)
	    ("C-x x p" . x509-view-paragraph-as-x509-certificate))))
 (eval-when-compile
   (use-package x509-certificate-region
     :if (command-exists-p "openssl")
     :quelpa (x509-certificate-region
              :fetcher github
              :repo "peterpaul/x509-certificate-region.el")
     :bind (("C-x x c" . x509-view-certificate)
            ("C-x x x" . x509-view-xml-element-as-x509-certificate)
            ("C-x x r" . x509-view-region-as-x509-certificate)
	    ("C-x x p" . x509-view-paragraph-as-x509-certificate)))))

(use-package keystore-mode
  :if (command-exists-p "keytool"))

;; https://github.com/stsquad/emacs_chrome
(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
            (edit-server-start)
          (add-hook 'after-init-hook
                    #'(lambda() (edit-server-start))))
  :config (progn
            (setq edit-server-new-frame nil)
            (setq edit-server-new-frame-alist
                  '((name . "Edit with Emacs FRAME")
                    (top . 200)
                    (left . 200)
                    (width . 80)
                    (height . 25)
                    (minibuffer . t)
                    (menu-bar-lines . t)
                    (window-system . x)))))

;; Start server if not running
(load "server")
(unless (server-running-p) (server-start))

(provide 'init)
;;; init ends here
