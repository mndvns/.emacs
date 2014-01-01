
(defun user--install-packages ()
  (packages-install
   '(evil
     surround
     multi-term
     rainbow-delimiters
     slime
     ac-slime)))

(condition-case nil
    (user--install-packages)
  (error
   (package-refresh-contents)
   (user--install-packages)))

;; Have a terminal. Have as many as you want.
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;; Turn on multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Turn off if things get slow.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(global-set-key (kbd "<f8>") 'rainbow-delimiters-mode)

;; Keep cursor away from edges when scrolling up/down.
;; Kinda growing on me.
(require 'smooth-scrolling)

;; Highlight current s-expression
(global-set-key (kbd "<f7>") 'hl-sexp-mode)
(hl-sexp-mode)

;; Be evil everywhere all the time
(setq-default evil-want-C-u-scroll t)
(setq-default evil-shift-width 2)
(setq-default global-surround-mode 1)
(evil-mode 1)

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

;; Default js indentation levels
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

;; No graphics
(setq speedbar-use-images nil)

;; Use GNU ls - install with:
;;    brew install xz coreutils
(setq insert-directory-program "gls")

()
