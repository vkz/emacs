(require 'smartparens-config)

(add-to-list 'sp-ignore-modes-list 'js2-mode)

;; (setq sp-autoskip-closing-pair 'always-end)

;; (smartparens-global-mode 1) 
;; (smartparens-global-mode -1)

;; (--each '(emacs-lisp-mode-hook)
;;   (add-hook it 'turn-on-smartparens-mode))

;; highlights matching pairs
(show-smartparens-global-mode t)

;; TODO enable strict mode
;; (smartparens-strict-mode +1)

;; TODO
;; https://github.com/Fuco1/smartparens/issues/446

(define-prefix-command 'sp-prefix-command)
(define-key sp-keymap (kbd "<C-i>") 'sp-prefix-command)
(define-key sp-keymap (kbd "C--") 'negative-argument)

;; Attempt 2
;; Baffling these bindings shadow all Meta combinations
;; (define-key sp-keymap (kbd "C--") 'negative-argument)

(define-key sp-keymap (kbd "C-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-b") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-(") 'sp-down-sexp)            ; -->(|
;; (define-key sp-keymap (kbd "M-(") 'sp-backward-up-sexp)     ;   |(<--
(define-key sp-keymap (kbd "C-{") 'sp-backward-up-sexp)     ;   |(<--

(define-key sp-keymap (kbd "C-)") 'sp-backward-down-sexp) ;   |)<--
(define-key sp-keymap (kbd "<C-i> C-(") 'sp-backward-down-sexp) ;   |)<--
;; (define-key sp-keymap (kbd "<C-[>") 'sp-backward-down-sexp) ;   |)<--
;; (define-key sp-keymap (kbd "M-[") 'sp-up-sexp)              ; -->)|
(define-key sp-keymap (kbd "<C-[>") 'sp-up-sexp)              ; -->)|

(define-key sp-keymap (kbd "C-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-p") 'sp-backward-sexp)

(require 'hydra)
(defhydra hydra-sp-nav-beginning ()
  "(|<-... )"
  ("n" sp-beginning-of-next-sexp "next beginning")
  ("p" sp-beginning-of-previous-sexp "previous beginning"))

(defhydra hydra-sp-nav-end ()
  "( ...->|)"
  ("n" sp-end-of-next-sexp "next end")
  ("p" sp-end-of-previous-sexp "previous end"))

(define-key sp-keymap (kbd "C-a") '(lambda () (interactive)
                                     (sp-beginning-of-sexp)
                                     (hydra-sp-nav-beginning/body)))

(define-key sp-keymap (kbd "C-e") '(lambda () (interactive)
                                     (sp-end-of-sexp)
                                     (hydra-sp-nav-end/body)))


;; editing commands
(define-key sp-keymap (kbd "M-d") 'sp-kill-sexp)
(define-key sp-keymap (kbd "M-h") 'sp-backward-kill-sexp)


;; TODO there's some craziness in how select commands work, so I'm keeping these
;; for now and ignoring the rest, but ought to figure them out
(define-key sp-keymap (kbd "M-f") 'sp-select-next-thing)
(define-key sp-keymap (kbd "M-b") 'sp-select-previous-thing-exchange)
;; (define-key sp-keymap (kbd "M-n") 'sp-select-next-thing-exchange)
;; (define-key sp-keymap (kbd "M-p") 'sp-select-previous-thing)

(define-key sp-keymap (kbd "M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "<C-i> w")
  (defhydra hydra-sp-edit-wrapping ()
    "wrapping"
    ("f" sp-unwrap-sexp "unwrap")
    ("b" (lambda (&optional arg)
           (interactive "p")
           (setq arg (or (and arg (- 0 arg)) -1))
           (sp-unwrap-sexp arg)) "-unwrap")
    ("r" sp-rewrap-sexp "rewrap")
    ("s" sp-swap-enclosing-sexp "swap enclosing")
    ("q" nil "quit")
    ("u" undo-tree-undo "undo")
    ("U" undo-tree-redo "redo")))

(define-key sp-keymap (kbd "<C-i> s")
  (defhydra hydra-sp-edit-splicing ()
    "splicing"
    ("h" sp-splice-sexp "splice")
    ("f" sp-splice-sexp-killing-forward "kill forward")
    ("b" sp-splice-sexp-killing-backward "kill backward")
    ("a" sp-splice-sexp-killing-around "kill around")
    ("q" nil "quit")
    ("u" undo-tree-undo "undo")
    ("U" undo-tree-redo "redo")))

(define-key sp-keymap (kbd "<C-i> b")
  (defhydra hydra-sp-edit-slurping ()
    "slupring & barfing"
    ("e(" sp-extract-before-sexp  "extract before")
    ("e[" sp-extract-after-sexp "extract after")
    ("s"  sp-split-sexp "split")
    ("j"  sp-join-sexp "join")
    ("["  sp-forward-slurp-sexp "slurp forward")
    ("]"  sp-forward-barf-sexp "barf forward")
    ("("  sp-backward-slurp-sexp "slurp backward")
    (")"  sp-backward-barf-sexp "barf backward")
    ("a(" sp-add-to-next-sexp "add to next")
    ("a[" sp-add-to-previous-sexp "add to previous")
    ("ab" sp-absorb-sexp "absorb")
    ("em" sp-emit-sexp "emit")
    ("q" nil "quit")
    ("u" undo-tree-undo "undo")
    ("U" undo-tree-redo "redo")))


(provide 'setup-smartparens)
