;;; -*- lexical-binding: t; -*-

(when (display-graphic-p)
  ;; (use-package smart-mode-line
  ;;   :config
  ;;   (sml/setup)
  ;;   )

  ;; (use-package powerline
  ;;   :config
  ;;   (powerline-default-theme)
  ;;   )
  
  ;; (defvar use-fancy-spaceline (y-or-n-p-with-timeout "Use fancy spaceline-all-the-icons?" 3 nil))
  (defvar use-fancy-spaceline t)
  
  (use-package spaceline)
  (use-package spaceline-config
    :ensure
    spaceline
    :config
    (unless use-fancy-spaceline
      ;;(spaceline-spacemacs-theme)
      (spaceline-emacs-theme)
      )
    )

  (when use-fancy-spaceline
    (use-package spaceline-all-the-icons
      :after spaceline
      :config
      (spaceline-all-the-icons-theme)
      (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
      (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
      (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
      (setq spaceline-all-the-icons-separator-type (quote wave))
      )
    )
  )

(provide 'customize-modeline)
;;; customize-modeline ends here
