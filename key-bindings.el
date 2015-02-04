;; TODO: consider binding <Enter> to Meta in Karabiner or something useful

;; Prefix should be central
(define-key global-map (kbd "C-t") (lookup-key global-map (kbd "C-x")))

;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
;; TODO 'delete-frame doesn't actually switch to the right (focused) frame when
;; there're more than one left.
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
(global-set-key (kbd "C-c i") 'helm-semantic-or-imenu)

;; helm for history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
(add-hook 'nodejs-repl-mode-hook
          #'(lambda ()
              (define-key nodejs-repl-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)))
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; Searching

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

;; Help
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "<f1> h") 'helm-apropos)

;; Killing stuff

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-S-w") (λ (mark-paragraph) (kill-region-or-backward-word)))
(global-set-key (kbd "M-h") 'kill-region-or-backward-word)
;; Kill lines but respect the indentation
(global-set-key (kbd "C-H") 'kill-and-retry-line)
(global-set-key (kbd "C-M-h") 'prelude-kill-whole-line)
;; Complement to C-k that also ignores the indentation
;; TODO do I actually need this?
(global-set-key (kbd "C-S-k") 'kill-to-beginning-of-line)
;; TODO: try actually using easy-kill features. Is easy-mark worth the effort?
;; TODO should properly work in major-modes like js2-mode, so it recognises defuns and stuff 
(global-set-key [remap kill-ring-save] 'easy-kill) ;M-w
;; Zap
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "M-z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))

;; Repeat last command
(global-set-key (kbd "<C-return>") 'repeat)

;; Windows, buffers, frames

(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "<backspace>") 'other-window)
(global-set-key (kbd "C-<tab>") 'other-window)
;; Toggle two most recent buffers
(global-set-key (kbd "C-<backspace>") 'quick-switch-buffer)
(global-set-key (kbd "S-<backspace>") 'other-frame)
(global-set-key (kbd "M-<backspace>") 'other-frame)
(global-set-key (kbd "<backtab>") 'other-frame)
(global-set-key (kbd "C-c <backspace>") 'prelude-swap-windows)
(global-set-key (kbd "C-c <tab>") 'prelude-swap-windows)

;; TODO: maybe I should just force off all window splitting functions
;; and simply always have horizontal split with two windows
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

;; Mighty unicode λ
;; TODO do I actually need this. Doesn't work in 'isearch-forward minibuffer.
(global-set-key (kbd "M-/") (λ (insert "\u03bb")))

;; Indentation help
;; TODO this one leaves a whitespace between chunks, is this what I want?
(global-set-key (kbd "C-+") 'prelude-top-join-line)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Navigation bindings
;; TODO 'smart-bindings are questionable do I really need these?
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; TODO: appears tha "C-i" is force-translated into TAB I guess to
;; replace not working TAB in terminals. Is there a way to take it
;; back? It is also bound to `forward button' in *Help*.

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(autoload 'magit-status "magit")

;; Expand region (increases selected region by semantic units)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-=") (λ (mark-paragraph) (exchange-point-and-mark) (backward-char)))

;; TODO: bind query-replace and friends

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; experimental
;; TODO are there better commands than pop-to-mark
(global-set-key (kbd "C-j") 'pop-to-mark-command)

;; alternate between beginning of text and line
(global-set-key [remap move-beginning-of-line] 'prelude-move-beginning-of-line)

;; Swap undo and universal argument
(define-key undo-tree-map (kbd "C-/") nil)
(define-key undo-tree-map (kbd "C-?") nil)
(global-set-key (kbd "C-u") 'undo-tree-undo)
(global-set-key (kbd "C-S-u") 'undo-tree-redo)
(global-set-key (kbd "C-M-u") 'undo-tree-visualize)
(global-set-key (kbd "C-/") 'universal-argument)
(global-set-key (kbd "C-?") 'negative-argument)

;; Duplicate region
(global-set-key (kbd "C-c d") 'prelude-duplicate-current-line-or-region)
(global-set-key (kbd "C-c D") 'prelude-duplicate-and-comment-current-line-or-region)

;; Jump from file to containing directory
(autoload 'dired-jump "dired")
(global-set-key (kbd "C-x C-j") (λ (dired-jump-other-window)))
(global-set-key (kbd "C-x j") (λ (dired-jump-other-window)))
(global-set-key (kbd "C-x J") 'dired-jump)

;; Shell
(global-set-key (kbd "C-c C-j") 'start-or-switch-to-shell)
(global-set-key (kbd "C-c j") 'shell-jump)
(global-set-key (kbd "C-c J") (λ (shell-jump t)))
(define-key dired-mode-map (kbd "C-c C-j") 'dired-shell-jump)
(define-key dired-mode-map (kbd "C-c j") 'dired-shell-jump)
(define-key helm-find-files-map (kbd "C-c C-j") 'helm-ff-shell-jump)
(define-key helm-find-files-map (kbd "C-c j") 'helm-ff-shell-jump)
(define-key shell-mode-map (kbd "C-c C-j") (λ (bury-buffer) (other-window 1)))

;; TODO is it worth wasting bindings on clean nodejs-repl?
(global-set-key (kbd "C-c C-n") 'start-or-switch-to-nodejs)
(global-set-key (kbd "C-c n") 'start-or-switch-to-nodejs)

;; Create scratch buffer and switch to it in other-window
(global-set-key (kbd "s-s") 'create-scratch-buffer)

;; TODO this maybe too close to <f1>
(global-set-key (kbd "<escape>") (λ (bury-buffer) (other-window 1)))
(global-set-key (kbd "S-<escape>") (λ (bury-buffer)))

;; Perspective
(define-key persp-mode-map (kbd "C-t p p") 'projectile-persp-switch-project)

;; Projectile
(global-set-key (kbd "C-c b") 'helm-projectile)

;; --------------------------------------------------------------
;; TODO can't isearch with "C-o" in dired buffers

;; TODO simpler binding for finding stuff in other-window from
;; helm. Make it consistend across all helm-maps. "<S-return>" or
;; "<C-return>" although it's already bound to repeat.
;; "<C-return>" to open in other-window, and "<M-return>" in
;; other-frame would make it consistend with my "<backspace>"
;; bindings.

(provide 'key-bindings)
