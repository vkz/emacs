(require 'helm-config)

;; TODO: `helm-locate-library' crashes Emacs when pattern starts with "j"
;; see `https://github.com/emacs-helm/helm/issues/779'

(setq helm-quick-update                     t
      helm-split-window-in-side-p           nil
      helm-split-window-default-side        'other
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     nil
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

;; https://github.com/emacs-helm/helm/issues/779
;; doesn't help, but I'll leave it be
(setq max-lisp-eval-depth 40000)
(setq max-specpdl-size 100000)

;; TODO: abomination! Either make it compeletely general or explicitly
;; swap key for helm only.
(defun my/setup-helm-prefix-key (key)
  "Replace the default helm prefix with `C-c h'."
  (when (and (boundp 'helm-command-prefix-key)
             (symbol-value 'helm-command-prefix-key))
    (global-unset-key (kbd helm-command-prefix-key)))
  (when key
    (global-set-key (kbd key) 'helm-command-prefix)))

;; TODO: should I replace `grep' with `ack-grep'?
;; TODO: man `find' for `helm-find'
;; TODO: man `locate' for `helm-locate'
;; TODO: `helm-lisp-completion-at-point' is pretty cool. Use it more often.
;; TODO: maybe worth turning completion off with helm-powered
;; completion as replacement.

(defun my/setup-helm-map ()
  "Customize helm-map bindings."
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-<tab>") 'helm-select-action)
  (define-key helm-map (kbd "C-t") (lookup-key helm-map (kbd "C-x"))))

(defun my/setup-helm-command-map ()
  "Customize helm-command-map."
  ;; TODO: many of these are worth rebinding. Not even sure it's worth
  ;; wasting "<SPS>" on mark-ring here, but that's a start. Do I even
  ;; use it? Simple pop-mark could be the way to go.
  (define-key helm-command-map (kbd "<SPC>") 'helm-all-mark-rings)
  (define-key helm-command-map (kbd "g") 'helm-do-grep)
  ;; TODO: bind `helm-locate-library'
  ;; TODO: rebind `helm-register' and learn registers.
  ;; TODO: rebind `helm-top' which is very cool, but deserves a less
  ;; useful key like `C-c t' or alike.
  )

;; Use helm for completion please
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)

;; helm-swoop
(require 'helm-swoop)
(setq helm-multi-swoop-edit-save t)
(setq helm-swoop-split-with-multiple-windows nil)
(setq helm-swoop-split-direction 'split-window-horizontally)
(setq helm-swoop-move-to-line-cycle nil)

(require 'helm-descbinds)
(require 'helm-eshell)
(helm-descbinds-mode)
(helm-mode 1)

(provide 'setup-helm)
