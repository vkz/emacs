(defun ze/this-file ()
  "Return path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))

(setq user-emacs-directory (file-name-directory (file-truename (ze/this-file))))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (file-exists-p (concat user-emacs-directory "elpa/archives/melpa"))
  (package-refresh-contents))

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
      whitespace-cleanup-mode
      elisp-slime-nav
      smooth-scrolling
      shell-command
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

(require 'appearance)

;; Write backup files to own directory
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      ;; Make backups of files, even when they're in version control
      vc-make-backup-files t)

;; Save point position between sessions
(use-package saveplace
  :init
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  (setq-default save-place t))

(setq view-read-only t)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (exec-path-from-shell-initialize))

(define-minor-mode ze-mode
  "Ze mode to override other bindings."
  :init-value nil
  :lighter " ze"
  :keymap (make-sparse-keymap "ze"))

(bind-keys :prefix-map ze-prefix
           :prefix "<f3>"
           :prefix-docstring
           "Prefix for counsel / buffers / filesystem / windows-layout commands")
(bind-keys :prefix-map ze-nav-prefix
           :prefix "M-t")

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
    (dired-next-line 2))

  ;; M-down is nicer in dired if it moves to the last file
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))

  (bind-keys :map dired-mode-map
             ("C-a" . dired-back-to-start-of-files)
             ([remap beginning-of-buffer] . dired-back-to-top)
             ([remap end-of-buffer] . dired-jump-to-bottom)))

(use-package dired-x
  :defer nil
  :after dired
  :bind (("C-x j" . dired-jump-other-window)
         ("C-x J" . dired-jump)))

(use-package wdired
  :after dired
  :config
  (bind-keys :map wdired-mode-map
             ("C-a" . dired-back-to-start-of-files)
             ([remap beginning-of-buffer] . dired-back-to-top)
             ([remap end-of-buffer] . dired-jump-to-bottom)))

(use-package dired+
  :ensure t
  :after dired)

(use-package neotree
  ;; Some useful features
  ;; https://www.emacswiki.org/emacs/NeoTree
  :ensure t
  :bind (:map ze-prefix
              ("T" . neotree-toggle))
  :config
  (setq neo-theme 'ascii
        neo-window-width 24
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line t
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-dont-be-alone t
        neo-persist-show nil
        neo-show-hidden-files t
        neo-auto-indent-point t
        neo-smart-open t)
  (apply
   #'custom-set-faces
   `((neo-file-link-face ((((background dark)) (:foreground "#839496"))
                          (((background light)) (:foreground "#555"))))
     (neo-dir-link-face ((t (:foreground "DodgerBlue1")))))))

;; Delete files to trash
(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(setq delete-by-moving-to-trash t)

(use-package magit
  :ensure t
  :init (bind-keys :prefix-map ze-vc-prefix
                   :prefix "<f2>")
  :bind (("<f2> c" . magit-clone)
         ("<f2> s" . magit-status)
         ("<f2> b" . magit-blame)
         ("<f2> l" . magit-log-buffer-file)
         ("<f2> p" . magit-pull)
         :map magit-mode-map
         ("<C-tab>" . nil)
         ("<M-tab>" . nil))
  :config
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package gist
  :ensure t
  :bind (("<f2> g" . gist-region-or-buffer-private)
         ("<f2> G" . gist-region-or-buffer)))

;; (eval-after-load 'shell '(require 'setup-shell))
(require 'setup-hippie)

(use-package yasnippet
  :ensure t
  :bind (("C-x t" . yas-expand))
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

;; (semantic-mode 1)

(setq max-lisp-eval-depth 40000)
(setq max-specpdl-size 100000)

(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :bind (
         :map ze-prefix
         ("r" . ivy-resume)
         ("b" . ivy-switch-buffer)
         ("B" . ivy-switch-buffer-other-window)
         :map ivy-minibuffer-map
         ("M-c" . ivy-kill-ring-save))
  :config
  (setq ivy-use-virtual-buffers t)
  :diminish ivy-mode)

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package swiper
  :ensure t
  :bind ())

(use-package counsel
  :ensure t
  :bind (("M-s" . counsel-grep-or-swiper)
         ("M-v" . counsel-yank-pop)
         ("C-:" . counsel-M-x)
         ([remap execute-extended-command] . counsel-M-x)
         ([remap find-file] . counsel-find-file)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
         ([remap describe-bindings] . counsel-descbinds)
         ("<f1> s" . counsel-info-lookup-symbol)
         :map ze-prefix
         ("f" . counsel-git)
         ("F" . counsel-find-file)
         ("R" . counsel-recentf)
         ("g" . counsel-git-grep)
         ("G" . counsel-ag)
         ("D" . counsel-dired-jump)
         ("L" . counsel-load-library)
         ("u" . counsel-unicode-char)
         ("m" . counsel-imenu)
         ("l" . counsel-locate)
         ("h" . counsel-outline)
         :map read-expression-map
         ("C-r" . counsel-expression-history)
         :map ze-nav-prefix
         ("h" . counsel-outline)))

(use-package "isearch"
  ;; Defer because `isearch' is not a feature and we don't want to `require' it
  :defer t
  :init
  (diminish 'isearch-mode)
  (setq isearch-allow-scroll t))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :bind (:map ze-prefix
              ("d" . projectile-find-dir)
              ("t" . gh/neotree-project-root))
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-completion-system 'ivy
        projectile-find-dir-includes-top-level t
        projectile-indexing-method 'alien
        projectile-enable-caching nil)
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".m2")
  (setq projectile-switch-project-action
        (lambda ()
          (dired (projectile-project-root))))

  (defun gh/neotree-project-root (&optional directory)
    "Open a NeoTree browser for a project DIRECTORY."
    (interactive)
    (let ((default-directory (or directory default-directory)))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (neotree-find (projectile-project-root)))))

  :diminish projectile-mode)

(use-package persp-mode
  :ensure t
  :bind (("<f3> SPC" . persp-key-map)
         ("<f3> <f3>" . persp-key-map))
  :init (persp-mode t)
  :config
  ;; (setq persp-common-buffer-filter-functions nil)

  ;; Leave C-c p to projectile, use <f4> instead
  (set-default 'persp-keymap-prefix (kbd "<f3> SPC"))
  ;; (substitute-key-definition 'persp-key-map nil persp-mode-map)

  ;; (add-hook 'persp-switch-hook
  ;;           (lambda ()
  ;;             (when (= (length (window-list)) 1)
  ;;               (with-selected-window (split-window-right)))))

  ;; stolen from Spacemacs
  ;; (defun ivy-persp-switch-project (arg)
  ;;   (interactive "P")
  ;;   (ivy-read "Switch to Project Perspective: "
  ;;             (if (projectile-project-p)
  ;;                 (cons (abbreviate-file-name (projectile-project-root))
  ;;                       (projectile-relevant-known-projects))
  ;;               projectile-known-projects)
  ;;             :action (lambda (project)
  ;;                       (let ((persp-reset-windows-on-nil-window-conf t))
  ;;                         (persp-switch project)
  ;;                         (let ((projectile-completion-system 'ivy))
  ;;                           (projectile-switch-project-by-name project))))))

  ;; (bind-keys :map persp-key-map
  ;;            ("S" . ivy-persp-switch-project))

  ;; (with-eval-after-load "ivy"
  ;;   (add-hook 'ivy-ignore-buffers
  ;;             #'(lambda (b)
  ;;                 (when persp-mode
  ;;                   (let ((persp (get-current-persp)))
  ;;                     (if persp
  ;;                         (not (persp-contain-buffer-p b persp))
  ;;                       nil)))))

  ;;   ;; (persp-contain-buffer-p (get-buffer ".emacs.d") (get-current-persp))
  ;;   ;; => nil
  ;;   ;; even though I have that dired buffer open

  ;;   (setq ivy-sort-functions-alist
  ;;         (append ivy-sort-functions-alist
  ;;                 '((persp-kill-buffer . nil)
  ;;                   (persp-remove-buffer . nil)
  ;;                   (persp-add-buffer . nil)
  ;;                   (persp-switch . nil)
  ;;                   (persp-window-switch . nil)
  ;;                   (persp-frame-switch . nil)))))
  ;; TODO ibuffer setup with persp
  ;; https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-mode-ibuffer-groups-el
  )

;; TODO: set these `grep-find-ignored-files' `grep-find-ignored-directories'
;; `projectile-globally-ignored-files' `projectile-globally-ignored-directories'
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

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Enable comment annotation keywords in programming modes
(comment-annotations-in-modes programming-modes)

(use-package expand-region
  :bind (("M-SPC" . er/expand-region))
  :ensure t
  :config
  (setq expand-region-contract-fast-key "-"
        expand-region-reset-fast-key "="))

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

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

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
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  :config
  (progn
    (setq lispy-safe-copy t
          lispy-safe-delete t
          lispy-safe-paste t
          lispy-visit-method 'projectile)
    (turn-on-smartparens-strict-mode)

    (defun sexy-kill-region-or-backward-word (arg)
      (interactive "p")
      (if (region-active-p)
          (lispy-kill-at-point)
        (lispy-backward-kill-word arg)))

    (defun sexy-move-beginning-of-line (arg)
      "Back to indentation, else fwd to `move-beginning-of-line'.
Reveal outlines."
      (interactive "^p")
      (lispy--ensure-visible)
      (prelude-move-beginning-of-line arg))

    (bind-keys :map lispy-mode-map
               ("C-a" . sexy-move-beginning-of-line)
               ("M-)" . lispy-parens-down)
               :map lispy-mode-map-lispy
               ;; was lispy-kill-at-point
               ("C-," . nil)
               ("<S-backspace>" . lispy-delete)
               ("C-w" . lispy-kill-at-point)
               ("M-h" . sexy-kill-region-or-backward-word))))

(use-package golden-ratio
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
                    other-window
                    ze-other-window
                    quit-window)))))

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
  :interpreter ("lua-5.1" . lua-mode)
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

(use-package launch
  :ensure t
  :init
  (global-launch-mode +1))

(use-package reveal-in-osx-finder
  ;; TODO binding
  :ensure t
  :bind (("C-c f" . reveal-in-osx-finder)))

(use-package avy-jump
  :ensure avy
  :bind (("M-t c" . avy-goto-char-timer)
         ("M-t M-c" . avy-goto-char-timer)
         ("M-t w" . avy-goto-word-1)
         ("M-t SPC" . avy-goto-word-1)
         ("M-t t" . avy-goto-word-1)
         ;; ("M-t j" . avy-pop-mark)
         ("M-t j" . pop-to-mark-command)
         ("M-t a" . beginning-of-buffer)
         ("M-t e" . end-of-buffer))
  :config
  (setq avy-timeout-seconds 0.3))

(use-package jump-char
  :ensure t
  :init (bind-keys*
         ;; note we are overriding widely used binding here. Will need to
         ;; carefully rebind it for modes that I use e.g. elisp-slime-nave etc.
         ("M-." . jump-char-forward)
         ("M-," . jump-char-backward))
  :config
  (setq-default jump-char-forward-key "."
                jump-char-backward-key ","))

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

(when is-mac
  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'super)
  (setq mac-right-control-modifier 'hyper)
  ;; (setq mac-option-modifier 'super)
  (setq mac-option-modifier nil))

(bind-keys
 ("C-<" . scroll-down-command)
 ("C->" . scroll-up-command)
 ("<escape>" . bury-buffer)
 ("C-x r q" . save-buffers-kill-terminal)
 ("C-x C-c" . delete-frame)
 ("C-t" . hippie-expand-no-case-fold)
 ;; Use default C-M-i till I find better key
 ;; ("M-t" . completion-at-point)
 ("<f1>" . help-command)
 ("M-h" . kill-region-or-backward-word)
 ("<C-tab>" . ze-other-window)
 ("<H-tab>" . other-frame)
 ("C-x <C-tab>" . i-meant-other-window)
 ("C-x 3" . split-window-right-and-move-there-dammit)
 ("C-c C-e" . eval-and-replace)
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
 ("<f9>" . kmacro-end-or-call-macro)
 ("C-'" . quoted-insert))

;; NOTE recover navigate to definition for major programming modes
(defun ze-navigate-to-definition ()
  (interactive)
  (case major-mode
    (emacs-lisp-mode (call-interactively #'elisp-slime-nav-find-elisp-thing-at-point))
    (racket-mode (call-interactively #'racket-visit-definition))
    ;; add cases for missing programming modes here
    ;; TODO clojure-mode
    (t (message (format "Navigate to definition missing for %s" major-mode)))))

(bind-keys ("M-t ." . ze-navigate-to-definition)
           ("M-t ," . pop-tag-mark))

;; Translate backward-delete onto C-h
(define-key key-translation-map [?\C-h] [?\C-?])
;; Translate keyboard-quit
(define-key key-translation-map [?\M-g] [?\C-g])
;; Translate kill-region
(define-key key-translation-map [?\M-w] [?\C-w])

;; Turn page breaks into lines
(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char))
  :config
  (setq zop-to-char-copy-keys '(?\M-c nil)
        zop-to-char-next-keys '(?\C-n nil)
        zop-to-char-prec-keys '(?\C-p nil)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command
        (mapconcat #'shell-quote-argument
                   `("pandoc" "--toc" "--section-divs"
                     "--standalone" "-f" "markdown" "-t" "html5")
                   " "))
  (defun ze-gfm-set-command ()
    (setq markdown-command
          (mapconcat #'shell-quote-argument
                     `("pandoc" "--toc" "--section-divs"
                       "--standalone" "-f" "markdown_github" "-t" "html5")
                     " ")))
  ;; set markdown-command to github flavor
  (add-hook 'gfm-mode-hook #'ze-gfm-set-command)
  ;; do not fill in gfm-mode
  (bind-key "M-q" #'ignore gfm-mode-map))

;;* org
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("<C-tab>" . nil)
         ("M-h" . nil)
         ("M-m" . org-mark-element))
  :config
  (setq org-directory "~/Dropbox/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list org-directory)
        org-archive-location (concat org-directory "/archive.org::* From %s"))
  (setq org-log-done t
        ;; display UTF-8 chars instead of escaped entities
        ;; e.g. \vert will show up as pipe (|)
        org-pretty-entities t
        ;; make headline indentation less noisy
        org-startup-indented t)

  ;; Add preview:path.pdf::NNN hyperlink to org. Now `org-insert-link' lets you
  ;; choose `preview:' hyperlink type that takes a path to PDF file followed by
  ;; `::NNN' page number. `org-open-at-point' will then start Preview.app on
  ;; that page.
  (org-add-link-type "preview" #'org-preview-open)

  (defun org-preview-open (path)
    "Given path-to-pdf::PAGE split into path and PAGE number. Use
AppleScript to open this pdf file in Preview.app at PAGE."
    (interactive)
    (let* ((path-page (split-string path "::"))
           (path (car path-page))
           (page (cadr path-page))
           ;; ensure absolute path
           (path (expand-file-name path))
           (script "osascript <<EOF

set posixFile to POSIX file \"%s\"
set pageNumber to %s

tell application \"Finder\"
	activate
	open posixFile
end tell

delay 1

tell application \"Preview\" to activate
delay 1
tell application \"System Events\"
	keystroke \"g\" using {option down, command down}
	keystroke pageNumber
	delay 1
	keystroke return
end tell

EOF
"))
      (save-window-excursion
        (async-shell-command
         (format script path page)))))
  ;; end
  )

(use-package ggtags
  ;; > brew install homebrew/emacs/ggtags
  ;; cd some-project/dir; gtags
  :ensure t
  :diminish ggtags-mode
  :init (progn
          (add-hook 'c-mode-common-hook
                    (lambda ()
                      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                        (ggtags-mode 1)))))
  :config
  (bind-keys :map ggtags-mode-map
             ("M-s" . nil)))

(add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))

(split-window-right)
;; (ze-toggle-golden-ratio)
