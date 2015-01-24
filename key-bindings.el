;; Use `C-t' which is a more natural prefix in Dvorak. Don't forget to
;; do the same for any minor modes that insist on having `C-x' as part
;; of their prefix
(define-key global-map (kbd "C-t") (lookup-key global-map (kbd "C-x")))

;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

(when is-mac
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; Completion

;; TODO find a good use for "C-," and "C-'" and "C-<tab>"
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-,") 'completion-at-point)
;; (global-set-key (kbd "C-'") 'hippie-expand-lines)

;; helm

(my/setup-helm-prefix-key "C-c h")
(my/setup-helm-command-map)
(my/setup-helm-map)
(global-set-key (kbd "C-t C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; helm for history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
(add-hook 'nodejs-repl-mode-hook
          #'(lambda ()
              (define-key nodejs-repl-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)))
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; Searching for stuff

;; Like isearch, but adds region (if any) to history and deactivates mark
(global-set-key (kbd "C-o") 'isearch-forward-use-region)
(global-set-key (kbd "C-S-o") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)
(global-set-key (kbd "C-S-r") 'isearch-backward-regexp)
;; Like isearch-*-use-region, but doesn't fuck with the active region
;; TODO why would I ever need these?
(global-set-key (kbd "s-o") 'isearch-forward)
(global-set-key (kbd "s-r") 'isearch-backward)
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-S-s") 'helm-multi-swoop-all)
(define-key isearch-mode-map (kbd "C-o") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-s") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "C-s") 'helm-multi-swoop-all-from-helm-swoop)
(define-key helm-command-map (kbd "s") 'helm-multi-swoop-all)

;; TODO: make good use of unused (kbd "H-SPC") - "LCtrl-SPC" on my MS keyboard
;; (global-set-key (kbd "H-SPC") 'rectangle-mark-mode)

;; experimental
;; TODO: consider binding <Enter> to Meta in Karabiner or something useful

;; experimental
;; Use shell-like backspace C-h, rebind help to F1
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "<f1> h") 'helm-apropos)
;; TODO: make sure 'helm-apropos and other helm helpers are bound

;; experimental
;; Killing stuff
;; TODO: with C-h and M-h bound to kill-backward try binding <backspace> to something useful
;; TODO: some smart use of C-k
;;
;; Idea-1: Killing lines with backspace:
;; -- <backspace> to 'kill-and-retry-line
;; -- C-<backspace> to kill entire line ??
;; -- M-<backspace> to kill to beginning of line ??
;;
;; Idea-2 easy-kill with backspace instead of difficult to type M-w
;; especially if I use its extended functionality. Unfortunately
;; typing any additional keys after <backspace> is not natural - palm
;; travels to much.
;;
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "M-h") 'kill-region-or-backward-word)
;; Kill lines but respect the indentation
(global-set-key (kbd "C-H") 'kill-and-retry-line)
(global-set-key (kbd "C-M-h") 'prelude-kill-whole-line)
;; Complement to C-k that also ignores the indentation
(global-set-key (kbd "C-c C-k") 'kill-to-beginning-of-line)
;; TODO: try actually using easy-kill features. Is easy-mark worth the effort?
;; TODO make work properly in major-modes like js2-mode, so it recognises defuns and stuff 
(global-set-key [remap kill-ring-save] 'easy-kill) ;M-w
;; (global-set-key [remap mark-sexp] 'easy-mark)

;; TODO: maybe I should just force off all window splitting functions
;; and simply always have horizontal split with two windows
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

;; swap two windows
(global-set-key (kbd "C-c s") 'prelude-swap-windows)

;; experimental
;; Zap to char
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "M-z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))

;; experimental
(global-set-key (kbd "<C-return>") 'repeat)

;; experimental
(global-set-key (kbd "C-c i") 'helm-semantic-or-imenu)

;; experimental
;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "<backspace>") 'other-window)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-<backspace>") 'other-frame)
(global-set-key (kbd "M-<backspace>") 'quick-switch-buffer)

;; Mighty unicode λ
;; TODO do I actually need this. Doesn't work in 'isearch-forward minibuffer.
(global-set-key (kbd "M-/") (λ (insert "\u03bb")))

;; Indentation help
;; TODO this one leaves a whitespace between chunks, is this what I want?
(global-set-key (kbd "C-+") 'prelude-top-join-line)

;; TODO: rebind C-- and C-+ to something more useful
;; TODO: map 'text-scale-decrease and 'text-scale-increase to some longer keystrokes

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Navigation bindings
;; 'smart-bindings are questionable
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; TODO: appears tha "C-i" is force-translated into TAB I guess to
;; replace not working TAB in terminals. Is there a way to take it
;; back? It is also bound to `forward button' in *Help*.

;; TODO: projectile
;; (define-key projectile-mode-map (kbd "C-c p p") nil)
;; (define-key projectile-mode-map (kbd "C-c p p") 'projectile-persp-switch-project)

;; TODO: better bindings for undo (undo-tree-undo,
;; undo-tree-visualize).  Somehow `undo-tree-map' steals "C-t u" so
;; that I can't take it back.

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(autoload 'magit-status "magit")

;; Expand region (increases selected region by semantic units)
(global-set-key (kbd "C-=") 'er/expand-region)
;; TODO I may want to have point at the region end instead of beginning in mark-paragraph
(global-set-key (kbd "M-=") 'mark-paragraph)
(global-set-key (kbd "C-M-=") 'mark-paragraph)

;; TODO: bind query-replace and friends

;; Comment/uncomment block
;; TODO: if no region comment/uncomment current line and move to the next line
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; experimental
;; TODO are there better commands than pop-to-mark
(global-set-key (kbd "C-j") 'pop-to-mark-command)

;; alternate between beginning of text and line
(global-set-key [remap move-beginning-of-line] 'prelude-move-beginning-of-line)

;; 'undo deserves a better key, 'universal-argument not so much
(define-key undo-tree-map (kbd "C-/") nil)
(define-key undo-tree-map (kbd "C-?") nil)
(global-set-key (kbd "C-u") 'undo-tree-undo)
(global-set-key (kbd "C-S-u") 'undo-tree-redo)
(global-set-key (kbd "C-M-u") 'undo-tree-visualize)
(global-set-key (kbd "C-/") 'universal-argument)
(global-set-key (kbd "C-?") 'negative-argument)


;; Create scratch buffer and switch to it in other-window
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Duplicate region
(global-set-key (kbd "C-c d") 'prelude-duplicate-current-line-or-region)
(global-set-key (kbd "C-c D") 'prelude-duplicate-and-comment-current-line-or-region)

;; TODO: Implement and bind toggle pairs () {} [] "" ''

;; Jump from file to containing directory
(autoload 'dired-jump "dired")
(global-set-key (kbd "C-x C-j") (λ (dired-jump 1)))
(global-set-key (kbd "C-x j") (λ (dired-jump 1)))
(global-set-key (kbd "C-x J") 'dired-jump)

;; Shell
(global-set-key (kbd "C-c C-j") 'start-or-switch-to-shell)
(global-set-key (kbd "C-c j") 'start-or-switch-to-shell)

;; TODO is it worth wasting bindings on clean nodejs-repl?
(global-set-key (kbd "C-c C-n") 'start-or-switch-to-nodejs)
(global-set-key (kbd "C-c n") 'start-or-switch-to-nodejs)

;; experimental
;; TODO this maybe too close to <f1>
(global-set-key (kbd "<escape>") (λ (bury-buffer) (other-window 1)))

(provide 'key-bindings)
