
(require 'evil)

(defun quick-term ()
  (interactive)
  (ansi-term "/bin/zsh")
  (linum-mode 0)
  (evil-emacs-state 1))

(global-set-key (kbd "H-t") 'quick-term)

(global-set-key (kbd "H-e") 'evil-emacs-state)
(global-set-key (kbd "H-E") 'evil-normal-state)

(setq confirm-nonexistent-file-or-buffer nil)

(provide 'keys-custom)
