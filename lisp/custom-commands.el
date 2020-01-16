;;; -*- lexical-binding: t; -*-

(defun dc02 ()
  (interactive)
  (async-shell-command "nmcli con up id dc02"))

(provide 'custom-commands)
;;; custom-commands ends here
