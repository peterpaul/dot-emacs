;;; -*- lexical-binding: t; -*-

(when (display-graphic-p)
  ;; (use-package smart-mode-line
  ;;   :config
  ;;   (sml/setup)
  ;;   )

  (use-package powerline
    :config
    (progn
      (setq powerline-utf-8-separator-left        #xe0b0
	    powerline-utf-8-separator-right       #xe0b2
	    airline-utf-glyph-separator-left      #xe0b0
	    airline-utf-glyph-separator-right     #xe0b2
	    airline-utf-glyph-subseparator-left   #xe0b1
	    airline-utf-glyph-subseparator-right  #xe0b3
	    airline-utf-glyph-branch              #xe0a0
	    airline-utf-glyph-readonly            #xe0a2
	    airline-utf-glyph-linenumber          #xe0a1)
      (powerline-default-theme)
      ))

  (use-package airline-themes
    :config
    (load-theme 'airline-solarized-gui)
    )
  
  ;; (defvar use-fancy-spaceline (y-or-n-p-with-timeout "Use fancy spaceline-all-the-icons?" 3 nil))
  ;; (defvar use-fancy-spaceline nil)

  ;; (use-package spaceline
  ;;   :config
  ;;   (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

  ;; (use-package spaceline-config
  ;;   :config
  ;;   (unless use-fancy-spaceline
  ;;     ;;(spaceline-spacemacs-theme)
  ;;     (spaceline-emacs-theme)
  ;;     )
  ;;   )

  ;; (when use-fancy-spaceline
  ;;   (use-package spaceline-all-the-icons
  ;;     :after spaceline
  ;;     :config
  ;;     (spaceline-all-the-icons-theme)
  ;;     (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  ;;     (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  ;;     (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  ;;     (setq spaceline-all-the-icons-separator-type (quote wave))
  ;;     )
  ;;   )
  )

(provide 'customize-modeline)
;;; customize-modeline ends here
