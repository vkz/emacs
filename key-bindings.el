;; this is XXI century PEOPLE ARE USING GUIs BY DEFAULT
;; take C-[ and C-i back and bind em usefully
;; `http://goo.gl/7Xmfn8'
;; TODO bind (kbd "C-<[>")
;; TODO bind (kbd "<C-<i>")

;; (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
;; (define-key input-decode-map [?\C-i] (kbd "<C-i>"))

;; Prefix should be central
(define-key global-map (kbd "C-t") (lookup-key global-map (kbd "C-x")))

;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
;; TODO 'delete-frame appears to misbehave when more than 1 frame left
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

(when is-mac
  (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier 'super)
  (setq mac-option-modifier nil))

;; Completion
(bind-keys ("C-." . set-mark-command)
           ("C-:" . helm-M-x)
           ("C-," . hippie-expand-no-case-fold)
           ("C-/" . completion-at-point)
           ;; :map iedit-mode-keymap
           ;; ("C-;" . nil)
           )

(global-set-key (kbd "C-'") 'hippie-expand-lines)

;; helm

(defun setup-helm-command-map-bindings ()
  "Bindings available after helm prefix."
  ;; TODO: many of these are worth rebinding
  ;; TODO: rebind `helm-register' and learn registers.
  (define-key helm-command-map (kbd "<SPC>") 'helm-all-mark-rings)
  (define-key helm-command-map (kbd "g") 'helm-do-grep))

(defun setup-helm-map-bindings ()
  "Customize `helm-map', which all helm sessions inherit. Includes
`helm-mini', `helm-projectile' and more."
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-<tab>") 'helm-select-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (define-key helm-map (kbd "C-t") (lookup-key helm-map (kbd "C-x")))
  (define-key helm-map (kbd "M-b") 'helm-mini/projectile-switch)
  (define-key helm-map (kbd "C-c b") 'helm-mini/projectile-switch)
  (define-key helm-map (kbd "C-x b") 'helm-mini/projectile-switch))

;; Find stuff in other-window replacing helm candidates by default. 
;; "Choice follows helm-window"
;; HACK <RET> in `helm-action' should have the default behavior
(defmacro maybe-helm-command-on-ret  (command default-action)
  `(defun ,(intern (concat "maybe-" (symbol-name command))) (&optional arg) 
     (interactive)
     (let* ((action (if (get-buffer helm-action-buffer)
                        #',default-action
                      #',command)))
       (funcall action))))

(maybe-helm-command-on-ret helm-buffer-switch-other-window helm-maybe-exit-minibuffer)
(maybe-helm-command-on-ret helm-ff-run-switch-other-window helm-maybe-exit-minibuffer)

(define-key helm-buffer-map (kbd "<RET>") 'maybe-helm-buffer-switch-other-window)
(define-key helm-buffer-map (kbd "<C-return>") 'helm-maybe-exit-minibuffer)

(define-key helm-find-files-map (kbd "<RET>") 'maybe-helm-ff-run-switch-other-window)
(define-key helm-find-files-map (kbd "<C-return>") 'helm-maybe-exit-minibuffer)

;; (define-key helm-projectile-find-file-map (kbd "<RET>") 'maybe-helm-ff-run-switch-other-window)
;; (define-key helm-projectile-find-file-map (kbd "<C-return>") 'helm-maybe-exit-minibuffer)

(define-key helm-generic-files-map (kbd "<RET>") 'maybe-helm-ff-run-switch-other-window)
(define-key helm-generic-files-map (kbd "<C-return>") 'helm-maybe-exit-minibuffer)

;; helm global
(global-unset-key (kbd helm-command-prefix-key))
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(setup-helm-command-map-bindings)
(setup-helm-map-bindings)
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
;; (global-set-key (kbd "C-o") 'isearch-forward-use-region)
(global-set-key (kbd "C-o") 'isearch-forward)

(global-set-key (kbd "C-S-o") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward)
(global-set-key (kbd "C-S-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-s") 'helm-swoop)
;; (global-set-key (kbd "C-S-s") 'helm-multi-swoop-all)
(define-key isearch-mode-map (kbd "C-o") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-s") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "C-s") 'helm-multi-swoop-all-from-helm-swoop)
(define-key helm-command-map (kbd "s") 'helm-multi-swoop-all)

;; Help
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "<f1> h") 'helm-apropos)
(global-set-key (kbd "S-<f1>") 'helm-apropos)
(global-set-key (kbd "M-<f1>") 'helm-apropos)

;; Killing stuff
;; TODO worth moving some of these into `easy-kill' if extending
;; it isn't too hard
(define-key key-translation-map [?\C-h] [?\C-?])
;; with `whole-line-or-region-mode' on will kill line when no region
(global-set-key (kbd "C-w") 'kill-region)
(global-set-key (kbd "C-S-w") (λ (mark-paragraph) (kill-region-or-backward-word)))
(global-set-key (kbd "M-h") 'kill-region-or-backward-word)
;; Kill lines but respect the indentation
(global-set-key (kbd "C-H") 'kill-and-retry-line)
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
(global-set-key (kbd "<backspace>") 'other-window)
(global-set-key (kbd "C-<tab>") 'other-window)
;; Toggle two most recent buffers
(global-set-key (kbd "C-<backspace>") 'quick-switch-buffer)
(global-set-key (kbd "S-<backspace>") 'other-frame)
(global-set-key (kbd "M-<backspace>") 'other-frame)
(global-set-key (kbd "<backtab>") 'other-frame)
(global-set-key (kbd "C-c <tab>") 'prelude-swap-windows)
(global-set-key (kbd "C-c <backspace>") 'i-meant-other-window)


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

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(autoload 'magit-status "magit")

;; Expand region (increases selected region by semantic units)
;; (global-set-key (kbd "C-=") 'er/expand-region)
;; (global-set-key (kbd "M-=") (λ (mark-paragraph) (exchange-point-and-mark) (backward-char)))

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
(define-key undo-tree-map (kbd "C-_") nil)
(global-set-key (kbd "C-u") 'undo-tree-undo)
(global-set-key (kbd "C-S-u") 'undo-tree-redo)
(global-set-key (kbd "C-M-u") 'undo-tree-visualize)
(global-set-key (kbd "C-t u") 'universal-argument)
(global-set-key (kbd "C-t U") 'negative-argument)


;; Duplicate region
(global-set-key (kbd "C-c d") 'prelude-duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-d") 'prelude-duplicate-and-comment-current-line-or-region)

(bind-keys ("C-x k" . kill-this-buffer)
           ("M-u" . ze-upcase-symbol-backwards))

;; Jump to shell
(global-set-key (kbd "C-c j") 'start-or-switch-to-shell)
(global-set-key (kbd "C-c J") (λ (start-or-switch-to-shell t)))
(define-key dired-mode-map (kbd "C-c j") 'dired-shell-jump)
(define-key helm-find-files-map (kbd "C-c j") 'helm-ff-shell-jump)
(define-key shell-mode-map (kbd "C-c j") 'shell-jump)

;; Jump to other processes
(global-set-key (kbd "C-c n") 'start-or-switch-to-nodejs)

;; Create scratch buffer and switch to it in other-window
(global-set-key (kbd "C-c s") 'create-scratch-buffer)

;; TODO this maybe too close to <f1>
(global-set-key (kbd "<escape>") (λ (bury-buffer) (other-window 1)))
(global-set-key (kbd "S-<escape>") (λ (bury-buffer)))

;; Perspective
(define-key persp-mode-map (kbd "C-t p p") 'projectile-persp-switch-project)

;; Projectile
(global-set-key (kbd "C-c b") 'helm-projectile)

(eval-after-load 'markdown-mode '(define-key markdown-mode-map (kbd "<backspace>") nil))


;; --------------------------------------------------------------

;; TODO simpler binding for finding stuff in other-window from
;; helm. Make it consistend across all helm-maps. "<S-return>" or
;; "<C-return>" although it's already bound to repeat.
;; "<C-return>" to open in other-window, and "<M-return>" in
;; other-frame would make it consistend with my "<backspace>"
;; bindings.

(provide 'key-bindings)
