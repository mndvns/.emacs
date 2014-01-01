
(require 'sws-mode)
(require 'evil)

(defun move-anyway nil
  "Move the fucking point to the fucking column."
  (interactive)
  (move-to-column
   (+ (current-column) 2) t))

(define-key evil-insert-mode-map [tab] 'sws-indent-line)
(define-key evil-insert-mode-map [backtab] 'sws-dendent-line)

(provide 'setup-sws-mode)
