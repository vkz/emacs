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

;; TODO: should I replace `grep' with `ack-grep'?
;; TODO: man `find' for `helm-find'
;; TODO: man `locate' for `helm-locate'
;; TODO: `helm-lisp-completion-at-point' is pretty cool. Use it more often.
;; TODO: maybe worth turning completion off with helm-powered
;; completion as replacement.

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
