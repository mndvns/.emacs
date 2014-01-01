
(require 'evil)
(require 'evil-numbers)
(require 'surround)

;; tab bullshit
(setq-default indent-tabs-mode nil)

;; Highlight current s-expression
(global-set-key (kbd "<f7>") 'hl-sexp-mode)
(hl-sexp-mode)

;; Be evil everywhere all the time
(setq evil-want-C-u-scroll t)
(setq-default evil-shift-width 2)
(global-surround-mode 1)
(evil-mode 1)

;; Increment & decrement
(define-key evil-normal-state-map (kbd "C-H-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-H-x") 'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; Insert-moves
(define-key evil-normal-state-map (kbd "C-k") 'smart-up)
(define-key evil-normal-state-map (kbd "C-j") 'smart-down)
(define-key evil-normal-state-map (kbd "C-l") 'smart-forward)
(define-key evil-normal-state-map (kbd "C-h") 'smart-backward)

;; Misc
(define-key evil-normal-state-map "-" 'delete-other-windows)
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
(define-key evil-normal-state-map (kbd "gc") 'comment-or-uncomment-region)
(define-key evil-normal-state-map (kbd "C-SPC") 'comment-or-uncomment-region)
(define-key evil-normal-state-map (kbd "C-w C-w") 'save-buffer-always)

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

;; Colored modes? Notbadface.jpg
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#ffffff" . "#5f8700"))
                                 ((evil-emacs-state-p)  '("#ffffff" . "#444488"))
                                 ((buffer-modified-p)   '("#ffffff" . "#0087ff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

(defun common-exit (prompt)
  "Functionality for escaping generally.Includes exiting Evil insert state and C-g binding. "
  (cond ((or (evil-insert-state-p)
             (evil-normal-state-p)
             (evil-replace-state-p)
             (evil-visual-state-p))
         [escape])
        (t (kbd "C-g"))))

;; Make escape vim-y.
(evil-define-command maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
         (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

;; We're gonna be doing some serious escaping.
(global-set-key [escape] 'keyboard-quit)
(define-key evil-insert-state-map "k" #'maybe-exit)
(define-key key-translation-map (kbd "C-c") 'my-esc)
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)

;; TODO get these things working before I die of anger
;; (define-key evil-visual-state-map (kbd "M-j")
;;   (lambda ()
;;     (interactive)
;;     (evil-normal-state)
;;     (open-line-above)
;;     (evil-visual-restore)))
;; (define-key evil-visual-state-map (kbd "M-k")
;;   (lambda ()
;;     (interactive)
;;     (evil-normal-state)
;;     (open-line-below)
;;     (evil-visual-restore)))

;; Better line openings.
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(define-key evil-normal-state-map "] " 'open-line-below)
(define-key evil-normal-state-map "[ " 'open-line-above)

(provide 'keys-evil)
