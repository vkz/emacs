(defun ze/this-file ()
  "Return path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))

(setq user-emacs-directory (file-name-directory (file-truename (ze/this-file))))
(add-to-list 'load-path user-emacs-directory)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Add external projects to load path. Note that anything installed
;; via package system will take precedence since dirs in elpa/ will
;; appear in `load-path' before site-lisp/ dirs and `package-install'
;; always loads files it installs. Either explicitly load customized
;; stuff before any packages or add their paths to `load-path' after
;; `package-initialize'. If ever in doubt which library took
;; precedence do `list-load-path-shadows'.
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar ze/required-packages
  '(dash)
  "Some packages are too good not to have.")

;; Setup packages
(require 'package)

;; Add melpa to package repos
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (file-exists-p (concat user-emacs-directory "elpa/archives/melpa"))
  (package-refresh-contents))

;; install required packages
(let ((install #'(lambda (package)
                   (unless (package-installed-p package)
                     (package-install package))
                   (require package))))
  (message "Installing required packages %s" ze/required-packages)
  (mapc install ze/required-packages)
  (delete-other-windows))

(defun packages-install (packages)
  (require 'dash)
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(use-package
      dash
      bash-completion
      guide-key
      highlight-escape-sequences
      whitespace-cleanup-mode
      elisp-slime-nav
      smooth-scrolling
      shell-command
      ;; expand-region
      ;; smart-forward
      js2-refactor
      js2-mode
      easy-kill
      rainbow-mode
      diminish
      smartparens
      eval-sexp-fu)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant
(require 'dash)
(require 'rx)

;; TODO learn dash, f, s, etc
(eval-after-load "dash" '(dash-enable-font-lock))

(use-package f
  :ensure t)

(use-package s
  :ensure t)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode
  :bind (("C-u" . undo-tree-undo)
         ("C-S-u" . undo-tree-redo)
         ("M-u" . undo-tree-visualize))
  :config (bind-keys :map undo-tree-map
                     ("C-/" . nil)
                     ("C-?" . nil)
                     ("C-_" . nil)))

;; Set up appearance early
(require 'appearance)

;; Write backup files to own directory
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file "backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      ;; Make backups of files, even when they're in version control
      vc-make-backup-files t)
;; (setq backup-directory-alist
;;       `(("." . ,(expand-file-name
;;                  (concat user-emacs-directory "backups")))))

;; Save point position between sessions
;; `saveplace' is part of Emacs
(use-package saveplace
  :init
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  (setq-default save-place t))

(setq view-read-only t)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (exec-path-from-shell-initialize))

;; dired
(use-package dired
  ;; TODO: learn fucking dired
  :config
  ;; brew install coreutils
  (when (and is-mac (executable-find "gls"))
    (setq insert-directory-program "gls"))

  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF"
        dired-ls-F-marks-symlinks t
        dired-recursive-copies 'always
        dired-dwim-target t)
  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    (setq dired-listing-switches
          (concat dired-listing-switches " -G -1 --group-directories-first -v")))

  (--each '(dired-do-rename
            dired-do-copy
            dired-create-directory
            wdired-abort-changes)
    (eval `(defadvice ,it (after revert-buffer activate)
             (revert-buffer))))

  ;; C-a is nicer in dired if it moves back to start of files
  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 2)))

  ;; M-up is nicer in dired if it moves to the fourth line - the first file
  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line 4))

  ;; M-down is nicer in dired if it moves to the last file
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1)))
(use-package dired
  ;; TODO: learn fucking dired
  :config
  ;; brew install coreutils
  (when (and is-mac (executable-find "gls"))
    (setq insert-directory-program "gls"))

  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF"
        dired-ls-F-marks-symlinks t
        dired-recursive-copies 'always
        dired-dwim-target t)
  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    (setq dired-listing-switches
          (concat dired-listing-switches " -G -1 --group-directories-first -v")))

  (--each '(dired-do-rename
            dired-do-copy
            dired-create-directory
            wdired-abort-changes)
    (eval `(defadvice ,it (after revert-buffer activate)
             (revert-buffer))))

  ;; C-a is nicer in dired if it moves back to start of files
  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 2)))

  ;; M-up is nicer in dired if it moves to the fourth line - the first file
  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line 4))

  ;; M-down is nicer in dired if it moves to the last file
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1)))

(use-package dired-x
  :defer nil
  :after dired
  :bind (("C-x j" . dired-jump-other-window)
         ("C-x J" . dired-jump)))

(progn
  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
  (define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom))

(use-package wdired
  :after dired
  :config
  (progn
     (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
     (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

(use-package dired+
  :ensure t
  :after dired)

(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

;; Delete files to trash
(setq delete-by-moving-to-trash t)

(use-package magit
  :ensure t
  :init (bind-keys :prefix-map ze-git-prefix-map
                   :prefix "<f2>")
  :bind (("<f2> c" . magit-clone)
         ("<f2> s" . magit-status)
         ("<f2> b" . magit-blame)
         ("<f2> l" . magit-log-buffer-file)
         ("<f2> p" . magit-pull))
  :config
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil))

(use-package gist
  :ensure t
  :bind (("<f2> g" . gist-region-or-buffer-private)
         ("<f2> G" . gist-region-or-buffer)))

(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-hippie)

(use-package yasnippet
  :ensure t
  :bind (("C-x ," . yas-expand))
  :init
  (progn
    (setq yas-verbosity 1
          yas-snippet-dirs `(,(expand-file-name "snippets" user-emacs-directory))
          yas-wrap-around-region t)

    (yas-global-mode 1))
  :config
  (progn
    ;; Inter-field navigation
    (defun yas/goto-end-of-active-field ()
      (interactive)
      (let* ((snippet (car (yas--snippets-at-point)))
             (position (yas--field-end (yas--snippet-active-field snippet))))
        (if (= (point) position)
            (move-end-of-line 1)
          (goto-char position))))

    (defun yas/goto-start-of-active-field ()
      (interactive)
      (let* ((snippet (car (yas--snippets-at-point)))
             (position (yas--field-start (yas--snippet-active-field snippet))))
        (if (= (point) position)
            (move-beginning-of-line 1)
          (goto-char position))))

    (bind-keys :map yas-keymap
               ("C-e" . yas/goto-end-of-active-field)
               ("C-a" . yas/goto-start-of-active-field)
               ("<return>" . yas/exit-all-snippets)
               :map yas-minor-mode-map
               ("<tab>" . nil)
               ("TAB" . nil)))
  :diminish yas-minor-mode)

(use-package datomic-snippets
  :ensure t)

;; TODO: `semantic-mode' appears to work with `c' `cpp' `js' but not
;; `elisp'. What major modes does it support? What features does it actually provide?
(semantic-mode 1)

;; helm
;; TODO ditch helm in favour of swiper?
;; https://github.com/emacs-helm/helm/issues/779
;; doesn't help, but I'll leave it be
(setq max-lisp-eval-depth 40000)
(setq max-specpdl-size 100000)

(use-package helm
  :ensure t
  :bind (;; ("s-h" . helm-command-prefix-key)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("TAB" . helm-execute-persistent-action)
         ("C-<tab>" . helm-select-action)
         ("C-." . helm-toggle-visible-mark))
  :init
  (helm-mode 1)
  (with-eval-after-load 'helm-config
    (warn "`helm-config' loaded! Get rid of it ASAP!"))
  :config
  (setq helm-quick-update t
        helm-split-window-in-side-p nil
        helm-split-window-default-side 'other
        helm-move-to-line-cycle-in-source nil)
  :diminish helm-mode)

(use-package helm-semantic
  :ensure helm
  :defer t
  :bind ("C-c i" . helm-semantic-or-imenu))

(use-package helm-ring
  :ensure helm
  :defer t
  :bind (("H-y" . helm-show-kill-ring)
         ;; ([remap insert-register] . helm-register)
         ))

(use-package helm-command
  :ensure helm
  :defer t
  :bind (("C-:" . helm-M-x)))

(use-package helm-buffers
  :ensure helm
  :defer t
  :bind (("C-x b" . helm-mini))
  :config
  (setq helm-buffers-fuzzy-matching t))

(use-package helm-files                 ; Manage files with Helm
  :ensure helm
  :defer t
  :bind (([remap find-file] . helm-find-files)
         ;; ("C-c f f" . helm-for-files)
         ;; ("C-c f r" . helm-recentf)
         )
  :config
  (setq helm-recentf-fuzzy-match t
        ;; Use recentf to manage file name history
        helm-ff-file-name-history-use-recentf t
        ;; Find libraries from `require', etc.
        helm-ff-search-library-in-sexp t)

  ;; (when (eq system-type 'darwin)
  ;;   ;; Replace locate with spotlight for `helm-for-files'
  ;;   (setq helm-for-files-preferred-list
  ;;         (append (delq 'helm-source-locate
  ;;                       helm-for-files-preferred-list)
  ;;                 '(helm-source-mac-spotlight))))
  )

(use-package helm-elisp
  :ensure helm
  :init (bind-keys ("<f1>" . help-command))
  :bind (("<f1> h" . helm-apropos)))

(use-package helm-descbinds
  :ensure t
  :init (helm-descbinds-mode))


;; Use helm for completion please
(use-package helm-c-yasnippet
  :ensure t
  :after helm
  :bind (("C-c y" . helm-yas-complete))
  :config
  (setq helm-yas-space-match-any-greedy t))

(use-package "isearch"                  ; Search buffers
  ;; Defer because `isearch' is not a feature and we don't want to `require' it
  :defer t
  :init
  ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
  ;; the feature name, but isearch.el does not provide any feature.  For the
  ;; same reason we have to use `:init', but isearch is always loaded anyways.
  (diminish 'isearch-mode)
  (setq isearch-allow-scroll t)
  (bind-keys :map isearch-mode-map
             ("<escape>" . isearch-abort)
             ("C-q" . isearch-abort)))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind (("s-s" . helm-swoop)
         :map helm-swoop-map
         ("C-u" . kill-to-beginning-of-line))
  :config
  (setq helm-swoop-speed-or-color t     ; Colour over speed 8)
        ;; Split window like Helm does
        helm-swoop-split-window-function #'helm-default-display-buffer)

  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows nil)
  (setq helm-swoop-split-direction 'split-window-horizontally)
  (setq helm-swoop-move-to-line-cycle nil))

(use-package helm-ag
  :ensure t
  :bind (("M-s" . helm-ag)
         ("M-S" . helm-do-ag))
  :config
  (setq helm-ag-fuzzy-match t
        helm-ag-insert-at-point 'symbol
        helm-ag-edit-save t))

;; projectile
(use-package projectile                 ; Project management for Emacs
  :ensure t
  :init (projectile-global-mode)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-completion-system 'helm
        projectile-find-dir-includes-top-level t
        projectile-indexing-method 'alien
        projectile-enable-caching nil)
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".m2")

  :diminish projectile-mode)

;; helm-projectile
(use-package helm-projectile
  :ensure t
  :after projectile
  :bind (("C-c b" . helm-projectile))
  :init
  (helm-projectile-on)
  :config
  (setq projectile-switch-project-action #'helm-projectile))

(use-package perspective
  :ensure t
  :bind (("<f4>" . perspective-map)
         :map perspective-map
         ("<f4>" . perspective-map)
         ("SPC" . persp-switch-last))
  :init (persp-mode t)
  :config
  (add-hook 'persp-switch-hook
            (lambda ()
              (when (= (length (window-list)) 1)
                (with-selected-window (split-window-right))))))

(use-package persp-projectile
  :ensure t
  :bind (:map perspective-map
         ("S" . projectile-persp-switch-project)
         ("s" . persp-switch)))

;; TODO: make sense of Virtual Directories
;; see `http://tuhdo.github.io/helm-projectile.html'

;; TODO: set these `grep-find-ignored-files' `grep-find-ignored-directories'
;; `projectile-globally-ignored-files' `projectile-globally-ignored-directories'
;; NOTE the need to do this to force `helm-projectile-grep' behave properly is
;; disconcerting see `https://github.com/bbatsov/projectile/issues/628'
(add-to-list 'grep-find-ignored-directories "elpa")
(add-to-list 'grep-find-ignored-directories "node_modules")


(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(add-hook 'js-mode-hook (lambda () (custom-set-default 'js-indent-level 2)))
(setq programming-modes
      '(clojure-mode js2-mode js-mode c-mode c++-mode emacs-lisp-mode racket-mode))

;; map files to modes
(require 'mode-mappings)

;; highlight escape sequences, works only in javascript
(use-package highlight-escape-sequences
  :ensure t
  :init (hes-mode))

;; (put 'font-lock-regexp-grouping-backslash 'face-alias 'js2-function-param)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Enable comment annotation keywords in programming modes
(comment-annotations-in-modes programming-modes)

(use-package expand-region
  ;; TODO bindings
  :bind (("C-r" . er/expand-region)
         ("M-r" . ze-mark-paragraph))
  :ensure t
  :init
  (defun ze-mark-paragraph ()
    (interactive)
    (progn
      (mark-paragraph)
      (exchange-point-and-mark)
      (backward-char)))
  :config
  (setq expand-region-contract-fast-key "R"
        expand-region-reset-fast-key "=")
  (er/line-wise-select-advice))
;; (require 'expand-region)
;; (er/line-wise-select-advice)

;; (require 'smart-forward)

;; (use-package eldoc
;;   :defer t
;;   :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
;;   :config
;;   (setq-default eldoc-documentation-function #'describe-char-eldoc)
;;   :diminish eldoc-mode)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (elisp-slime-nav-mode t)
            (eldoc-mode 1)
            (rainbow-mode +1)))

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))
;; (global-whitespace-mode +1)
;; (global-whitespace-mode -1)

(require 'smartparens-config)
(add-hook 'js-mode-hook 'turn-on-smartparens-mode)
(add-hook 'js-mode-hook 'show-smartparens-mode)
(show-smartparens-global-mode)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'"
                              "/elpa/.*\\'"
                              "/\\.node_modules/.*\\'"
                              ;; #'ignoramus-boring-p
                              )))

;; revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :init (global-auto-revert-mode t)
  :config
  (setq auto-revert-verbose nil
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)
  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil))
  :diminish auto-revert-mode)

(use-package re-builder
  :defer t
  :config (setq reb-re-syntax 'rx))

;; Does package really have anything to do with `require` though? I would've though that all it does having installed the package is add it's directory to the **load-path**. Having to `(require 'helm-config)` before installing the anything else seems really dissatisfying if only for the fact that Emacs doesn't make a good use of information that it already has. Also, having
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; OCaml
;; `https://github.com/realworldocaml/book/wiki/Installation-Instructions'
;; brew install opam
;; opam install core utop merlin ocp-indent
(use-package opam                       ; Initialize Emacs with OPAM env
  :ensure t
  :init (opam-init))

(use-package tuareg                     ; OCaml editing
  :ensure t
  :defer t
  :config
  (progn
    ;; Disable SMIE indentation in Tuareg.  It's just broken currently…
    (setq tuareg-use-smie nil)

    ;; Please, Tuareg, don't kill my imenu
    (define-key tuareg-mode-map [?\C-c ?i] nil)

    ;; Also BACKSPACE, geeez
    (define-key tuareg-mode-map (kbd "<backspace>") nil)
    (define-key tuareg-mode-map (kbd "C-<backspace>") nil)

    ;; Gimme better indentation experience
    (use-package ocp-indent
      :ensure t)))

(use-package merlin                     ; Powerful Emacs backend for OCaml
  :ensure t
  :defer t
  :init (add-hook 'tuareg-mode-hook #'merlin-mode)
  :config
  ;; Use Merlin from current OPAM env
  (setq merlin-command 'opam
        ;; Disable Merlin's own error checking in favour of Flycheck
        merlin-error-after-save nil))

(use-package utop
  :ensure t
  :defer t
  :init (add-hook 'tuareg-mode-hook #'utop-minor-mode))

;; Elisp
(use-package elisp-mode
  :interpreter ("emacs" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-defun)
              ("C-c C-e" . eval-last-sexp)
              ("C-c C-k" . eval-buffer)))

(use-package macrostep
  :ensure t
  :after elisp-mode
  :bind (:map emacs-lisp-mode-map ("C-c m" . macrostep-expand)
         :map lisp-interaction-mode-map ("C-c m" . macrostep-expand)))

;; lispy.el
(use-package lispy
  :ensure t
  :defer t
  :diminish
  (lispy-mode . "(λ)")
  :init
  (progn
    (defun ze-emacs-lisp-hook ()
      (lispy-mode 1))

    (add-hook 'emacs-lisp-mode-hook 'ze-emacs-lisp-hook))
  :config
  (progn
    (defun sexy-form-state-p ()
      (or (region-active-p)
          (and (not (lispy--in-string-or-comment-p))
               (or (looking-back "['`~@#]")
                   (and (looking-at "\\(?:[[:space:]]+\\)")
                        (or (looking-back lispy-left)
                            (looking-back "\\(?:^\\|[[:space:]]+\\)")))
                   (and (looking-at lispy-right)
                        (or (looking-back "\\(?:^\\|[[:space:]]+\\)")
                            (looking-back lispy-left)))
                   (and (looking-at "\\(?:$\\|[[:space:]]+\\)")
                        (looking-back "\\(?:^\\|[[:space:]]+\\)"))))))
    (defun sexy-parens ()
      (interactive)
      (cond
       ((sexy-form-state-p) (call-interactively 'lispy-parens))
       (t (call-interactively 'lispy-backward))))

    (defun sexy--skip-string-or-comment-down (arg)
      (interactive "p")
      (cond ((lispy--in-string-p)
             (goto-char (cdr (lispy--bounds-string))))
            ((lispy--in-comment-p)
             (progn
               (goto-char (1+ (cdr (lispy--bounds-comment))))
               (skip-chars-forward "\n")
               (forward-sexp 1)
               (forward-sexp -1)))))

    (defun sexy--skip-string-or-comment-up (arg)
      (interactive "p")
      (cond ((lispy--in-string-p)
             (goto-char (car (lispy--bounds-string))))
            ((lispy--in-comment-p)
             (progn
               (goto-char (1- ((lispy--bounds-comment))))
               (skip-chars-backward "\n")
               (forward-sexp -1)
               (forward-sexp 1)))))

    (defun sexy-try ()
      ""
      (interactive)
      (when (lispy-looking-back "\\sw #")
        (progn
          (backward-delete-char 2)
          (insert "#"))))

    (defun sexy-forward-line-column ()
      (interactive)
      (let ((c (current-column)))
        (forward-line 1)
        (move-to-column c)
        (= c (current-column))))

    (defun sexy-find-lispy-left-down (arg)
      (interactive "p")
      (let ((p (point))
            (c (current-column))
            (lispy-thing (cond
                          ((lispy-left-p) #'lispy-left-p)
                          ((lispy-right-p) #'lispy-right-p)
                          (t nil)))
            (end (buffer-end 1))
            (sexy-found-p nil))
        (when lispy-thing
          (while (and (not sexy-found-p)
                      (not (= (point) end))
                      (forward-line 1))
            (when (and (= c (move-to-column c))
                       (funcall lispy-thing))
              (setq sexy-found-p t)))
          (when (not (and (funcall lispy-thing)
                          (= c (current-column))))
            (goto-char p)))))

    (defun sexy-into-sexp (arg)
      (interactive "p")
      (cond ((lispy-right-p)
             (backward-char 1)
             (insert " "))
            ((lispy-left-p)
             (forward-char 1)
             (insert " ")
             (backward-char 1))))

    (defun sexy-tab (arg)
      ""
      (interactive "p")
      (lispy-tab)
      (indent-for-tab-command arg))

    ;; Experimental lispy bindings
    (lispy-define-key lispy-mode-map-special "i" 'sexy-into-sexp)
    (bind-keys :map lispy-mode-map
               ("<tab>" . sexy-tab)
               ;; ("<C-i>" . sexy-tab)
               )
    ;; (lispy-define-key lispy-mode-map "<C-i>" 'sexy-tab)

    (lispy-define-key lispy-mode-map "<tab>" 'sexy-tab)

    (defun sexy-next-thing (arg)
      ""
      (interactive "p")
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (sexy--skip-string-or-comment-down 1)
      (forward-sexp 1))

    (defun sexy-previous-thing (arg)
      ""
      (interactive "p")
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (sexy--skip-string-or-comment-up 1)
      (backward-sexp 1))

    (defun sexy-next-thing (arg)
      ""
      (interactive "p")
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (sexy-out-of-string-or-comment 1)
      (forward-sexp 1))

    (defun sexy-next-thing (arg)
      ""
      (interactive "p")
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (when (lispy--in-string-or-comment-p)
        (forward-sexp))
      (unless (looking-at-p lispy-right)
        (sp-select-next-thing)
        (lispy-down arg)
        (when (region-active-p)
          (deactivate-mark))))

    (defun sexy-previous-thing (arg)
      ""
      (interactive "p")
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (unless (looking-back lispy-left)
        (sp-select-previous-thing)
        (lispy-up arg)
        (when (region-active-p)
          (deactivate-mark))))

    (defun sexy-prev-paragraph ()
      ""
      (interactive)
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (if (= (point)
             (progn (lispy-beginning-of-defun)
                    (point)))
          (lispy-up 1)))

    (defun sexy-next-paragraph ()
      ""
      (interactive)
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (lispy-beginning-of-defun)
      (lispy-down 1))

    (turn-on-smartparens-strict-mode)
    (setq lispy-visit-method 'projectile)
    (bind-keys :map lispy-other-mode-map
               ("j" . nil)
               ("k" . nil)
               ("p" . nil)
               ("n" . nil)
               ("(" . special-lispy-beginning-of-defun))
    (bind-keys :map lispy-mode-map-lispy
               ("C-j" . nil)
               ("C-," . nil)
               ("<C-return>" . nil))

    ;; make sexy-..-paragraph special
    (lispy-define-key lispy-mode-map "j" 'sexy-next-paragraph)
    (lispy-define-key lispy-mode-map "k" 'sexy-prev-paragraph)

    (bind-keys :map lispy-mode-map
               ("M-n" . sexy-next-paragraph)
               ("M-p" . sexy-prev-paragraph)
               ("<C-return>" . nil)
               ("C-," . nil)
               ("M-j" . nil)
               ("(" . sexy-parens)
               (")" . lispy-right)

               ("C-(" . lispy-parens-down)
               ("C-)" . lispy-out-forward-newline)

               ("n" . special-lispy-down)
               ("p" . special-lispy-up)

               ;; ("j" . special-lispy-down)
               ;; ("k" . special-lispy-up)

               ("C-c x" . lispy-x)
               ("x" . special-lispy-x)

               ("C-k" . lispy-kill)
               ("C-w" . lispy-kill-at-point)

               ("C" . special-lispy-clone)
               ("c" . special-lispy-new-copy)

               ("N" . special-lispy-move-down)
               ("P" . special-lispy-move-up)

               ("C-c h i" . lispy-describe-inline)
               ("C-c h a" . lispy-arglist-inline)

               ("s" . special-lispy-ace-symbol)
               ("S" . special-lispy-ace-symbol-replace)
               ("a" . special-lispy-ace-paren)

               ("K" . nil)
               ("J" . nil)
               ;; ("k" . nil)
               ;; ("j" . nil)
               ("<C-i>" . nil)


               ("[" . lispy-brackets)
               ("C-u" . undo-tree-undo)

               ;; ("/" . special-lispy-splice)
               ("/" . nil)
               ("M-(" . nil)
               ("M-)" . nil)
               ("{" . nil)
               ("}" . nil)
               ("C-M-a" . sp-beginning-of-sexp)
               ("C-M-e" . sp-end-of-sexp)

               ("C-<down>" . sp-down-sexp)
               ("C-<up>" . sp-up-sexp)
               ("M-<down>" . sp-backward-down-sexp)
               ("M-<up>" . sp-backward-up-sexp)

               ("C-M-f" . nil)
               ("C-M-b" . nil)
               ("C-S-n" . nil)
               ("C-S-p" . nil)
               ("C-S-f" . nil)
               ("C-S-b" . nil)

               ("C-<right>" . sp-forward-slurp-sexp)
               ("M-<right>" . sp-forward-barf-sexp)
               ("C-<left>" . sp-backward-slurp-sexp)
               ("M-<left>" . sp-backward-barf-sexp)

               ("C-M-t" . sp-transpose-sexp)
               ("C-M-w" . nil)

               ("C-M-d" . delete-sexp)

               ("M-<backspace>" . nil)
               ("C-<backspace>" . nil)
               ("M-h" . nil)
               ([remap sp-backward-kill-word] . backward-kill-word)

               ("M-[" . sp-backward-unwrap-sexp)
               ("M-]" . sp-unwrap-sexp)

               ("C-x C-t" . sp-transpose-hybrid-sexp)

               ("]" . sp-rewrap-sexp))))

(use-package golden-ratio
  ;; TODO fix golden-ratio for shell and dired
  :ensure t
  :init
  (defun ze-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :diminish golden-ratio-mode
  :config
  (progn
    (setq golden-ratio-exclude-modes '("bs-mode"
                                       "calc-mode"
                                       "ediff-mode"
                                       "gud-mode"
                                       "gdb-locals-mode"
                                       "gdb-registers-mode"
                                       "gdb-breakpoints-mode"
                                       "gdb-threads-mode"
                                       "gdb-frames-mode"
                                       "gdb-inferior-io-mode"
                                       "gud-mode"
                                       "gdb-inferior-io-mode"
                                       "gdb-disassembly-mode"
                                       "gdb-memory-mode"
                                       "restclient-mode"
                                       "speedbar-mode"
                                       ))

    (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

    (setq golden-ratio-extra-commands
          (append golden-ratio-extra-commands
                  '(ace-window
                    ace-delete-window
                    ace-select-window
                    ace-swap-window
                    ace-maximize-window
                    avy-pop-mark
                    windmove-left
                    windmove-right
                    windmove-up
                    windmove-down
                    select-window-0
                    select-window-1
                    select-window-2
                    select-window-3
                    select-window-4
                    select-window-5
                    select-window-6
                    select-window-7
                    select-window-8
                    select-window-9
                    buf-move-left
                    buf-move-right
                    buf-move-up
                    buf-move-down
                    ess-eval-buffer-and-go
                    ess-eval-function-and-go
                    ess-eval-line-and-go
                    other-window)))))

;; clojure
(require 'setup-clj)

(use-package racket-mode
  ;; :ensure t
  :load-path "site-lisp/racket-mode/"
  :mode (("\\.rkt\\'" . racket-mode))
  :config
  (add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))
  ;; :config (progn
  ;;           (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  ;;           (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))

  ;; (lookup-key racket-mode-map (kbd "C-c C-e"))
  (bind-keys :map racket-mode-map
             ;; ("C-c m" . racket-macro-expand-map)
             ("C-c C-c" . racket-send-definition)
             ("C-c C-e" . racket-send-last-sexp)))

;; (use-package geiser
;;   :ensure t
;;   :bind (:map geiser-mode-map
;;               ("C-." . nil))
;;   :config
;;   (setq geiser-racket-collects
;;         '("/Users/zerusski/Library/Racket/6.4/collects"
;;           "/opt/homebrew-cask/Caskroom/racket/6.4/Racket v6.4/collects"
;;           "/Users/zerusski/Library/Racket/6.4/pkgs"
;;           "/Users/zerusski/Library/Racket/planet/300/packages")))

;; (use-package pdf-mode
;;   :load-path "site-lisp/pdf-mode.el/"
;;   :mode (("\\.rkt\\'" . pdf-mode)))

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode)
         ("\\.t\\'" . lua-mode))
  ;; TODO replace or rather add terra repl. lua-mode hardcodes most of the stuff
  ;; including interpreter and mode alists which is annoying, but I think I
  ;; should be able to extend functionality to terra pretty easily. E.g.
  ;; (lua-start-process "terra" "terra") will happily start terra repl, of
  ;; course sending stuff their doesn't seem to work out of the box.
  :interpreter ("lua-5.3" . lua-mode)
  :config
  ;; TODO wait, I'm not running company-mode? How do i get my completions then?
  ;; TODO there's some work for eldoc support in lua. I'd like to have that as
  ;; well as terra. (add-hook 'lua-mode-hook 'company-mode)
  (progn
    (setq lua-indent-level 2
          lua-indent-string-contents t)
    ;; ('lua-search-documentation)
    ;; ( 'lua-send-buffer)
    ;; ( 'lua-send-defun)
    ;; ( 'lua-send-current-line)
    ;; ( 'lua-send-region)
    ))

;; (setq interpreter-mode-alist (rassq-delete-all 'lua-mode interpreter-mode-alist))


(use-package launch
  :ensure t
  :init
  (global-launch-mode +1))

(use-package reveal-in-osx-finder
  :ensure t
  :bind (("C-c f" . reveal-in-osx-finder)))

(use-package avy-jump
  :ensure avy
  :init (eval-after-load "isearch"
          '(define-key isearch-mode-map (kbd "M-g") 'avy-isearch))
  :bind (("M-g c" . avy-goto-char-timer)
         ("M-g M-c" . avy-goto-char-timer)
         ("M-g w" . avy-goto-word-1)
         ;; ("M-g j" . avy-pop-mark)
         ("M-g j" . pop-to-mark-command)
         ("M-g a" . beginning-of-buffer)
         ("M-g e" . end-of-buffer))
  :config
  (setq avy-timeout-seconds 0.3))

(use-package jump-char
  ;; TODO fix and use it's integration with isearch and ace-jump (replacing it
  ;; with avy first)
  :ensure t
  :bind (("C-S-f" . jump-char-forward)
         ;; right command
         ("C-S-b" . jump-char-backward)
         ;; left command
         )
  :config
  ;; TODO I'd rather , and ; have vim liske meaning i.e. , continue in the
  ;; direction of original jump-char command and ; take the opposite direction.
  ;; Always forwand and always backward seems rigid. But it may just be a matter
  ;; of getting used to.
  (setq-default jump-char-forward-key ","
                jump-char-backward-key ";"))

;; TODO buggy due to use of overriding-local-map, after jumping due to the way
;; the overriding-local-map works only it and global-key-map are availabel,
;; therefore any bindings from minor mode u expect aren't available. Solution
;; could be extending the the way the passthrough works i.e. for it to work for
;; anything that's not iy specific binding. Even better solution is to avoid
;; local map altogether.
(use-package iy-go-to-char
  :disabled t
  :ensure t
  ;; With COMMAND keys translated by Karabiner these bindings work pretty nicely
  :bind (("C-S-f" . iy-go-up-to-char)
         ;; right command
         ("C-S-b" . iy-go-to-char-backward)
         ;; left command
         )
  :config
  (bind-keys :map iy-go-to-char-keymap
             ("C-s" . iy-go-to-char-isearch)
             ("C-r" . iy-go-to-char-isearch-backward)
             ("M-w" . nil)
             ("C-w" . iy-go-to-char-kill-region)
             ("M-c" . iy-go-to-char-kill-ring-save)
             ("C-g" . iy-go-to-char-quit))
  (setq iy-go-to-char-key-forward (kbd ","))
  (setq iy-go-to-char-key-backward (kbd ";")))

;; TODO iedit?
;; TODO bindings
(use-package multiple-cursors
  :ensure t
  :bind (("s-m" . mc/mark-more-like-this-extended)
         ("s-M" . mc/mark-all-like-this-dwim)
         ;; requires visual-regexp package
         ;; ("r" . vr/mc-mark)
         ("s-r" . mc/mark-all-in-region))
  :config
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))

;; (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
;; (define-key input-decode-map [?\C-i] (kbd "<C-i>"))

;; (λ (start-or-switch-to-shell t))
;; (λ (bury-buffer))

(when is-mac
  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'super)
  (setq mac-right-control-modifier 'hyper)
  ;; (setq mac-option-modifier 'super)
  (setq mac-option-modifier nil))

(bind-keys
 ("<escape>" . keyboard-quit)
 ("C-x r q" . save-buffers-kill-terminal)
 ("C-x C-c" . delete-frame)
 ("C-." . set-mark-command)
 ("C-," . hippie-expand-no-case-fold)
 ;; TODO gets overwritten by iedit mode
 ("C-;" . completion-at-point)
 ("<f1>" . help-command)
 ("M-h" . kill-region-or-backward-word)
 ("<C-return>" . repeat)
 ("<backspace>" . other-window)
 ("C-<backspace>" . quick-switch-buffer)
 ("M-<backspace>" . other-frame)
 ("<backtab>" . other-frame)
 ("C-c <tab>" . prelude-swap-windows)
 ("C-c <backspace>" . i-meant-other-window)
 ("C-x 3" . split-window-right-and-move-there-dammit)
 ("C-c C-e" . eval-and-replace)
 ("M-p" . backward-paragraph)
 ("M-n" . forward-paragraph)
 ("C-c c" . comment-or-uncomment-region-or-line)
 ("C-c d" . prelude-duplicate-current-line-or-region)
 ("H-j" . pop-to-mark-command)
 ("H-u" . universal-argument)
 ("H-S-u" . negative-argument)
 ("C-c M-d" . prelude-duplicate-and-comment-current-line-or-region)
 ("C-c j" . start-or-switch-to-shell)
 ("C-c s" . create-scratch-buffer)
 ("M-c" . easy-kill)
 ("C-a" . prelude-move-beginning-of-line)
 ("C-x k" . kill-this-buffer)
 ("<f8>" . kmacro-start-macro-or-insert-counter)
 ("<f9>" . kmacro-end-or-call-macro))

(bind-keys*
 ("C-;" . completion-at-point))

;; Move DEL to C-h
;; NOTE these remappings ought to occur in this order
;; remap quoted-insert onto C-'
;; (define-key key-translation-map (kbd "C-'") (kbd "C-q"))
;; remap keyboard-quit onto C-q
;; (define-key key-translation-map (kbd "C-q") (kbd "C-g"))
;; remap backward-delete onto C-h
(define-key key-translation-map [?\C-h] [?\C-?])

(split-window-right)
(ze-toggle-golden-ratio)
