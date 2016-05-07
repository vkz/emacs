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
(load custom-file)

(defvar ze/required-packages
  '(dash)
  "Some packages are too good not to have.")

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(use-package
      dash
      bash-completion
      magit
      guide-key
      highlight-escape-sequences
      whitespace-cleanup-mode
      elisp-slime-nav
      smooth-scrolling
      undo-tree
      shell-command
      helm
      helm-descbinds
      ;; helm-c-yasnippet
      expand-region
      ;; smart-forward
      js2-refactor
      js2-mode
      easy-kill
      rainbow-mode
      diminish
      whole-line-or-region
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

;; Set up appearance early
(require 'appearance)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
;; `saveplace' is part of Emacs
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

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

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-t r" "C-t 4" "C-t v" "C-c h" "C-c p" "C-t p" "C-c C-r" "<f1>"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'right)

;; dired
(use-package dired
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

;; TODO: learn the many features
(use-package dired+
  :ensure t
  :after dired)

(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

;; Delete files to trash
(setq delete-by-moving-to-trash t)

;; Setup extensions
(require 'setup-magit)
(eval-after-load 'shell '(require 'setup-shell)) ; TODO: helm support for completion
(require 'setup-hippie)                          ;TODO: practice all expansion methods

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
(require 'setup-helm)
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
  :bind (;; ("C-c s" . helm-projectile-ag)
         :map helm-projectile-find-file-map
         ("<RET>" . maybe-helm-ff-run-switch-other-window)
         ("<C-return>" . helm-maybe-exit-minibuffer))
  :init
  (helm-projectile-on)
  :config
  (setq projectile-switch-project-action #'helm-projectile))

(use-package perspective
  :ensure t
  :bind (("<f4>" . persp-switch-last)
         ("<f3>" . perspective-map))
  :init (persp-mode t)
  :config
  (add-hook 'persp-switch-hook
            (lambda ()
              (when (= (length (window-list)) 1)
                (with-selected-window (split-window-right))))))

(use-package persp-projectile
  :ensure t
  :bind (("S-<f3>" . projectile-persp-switch-project)
         :map perspective-map
         ("s" . projectile-persp-switch-project)
         ("S" . persp-switch))) 

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
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'js2-function-param)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Enable comment annotation keywords in programming modes
(comment-annotations-in-modes programming-modes)

(use-package expand-region
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

;; basic commands act on a whole line with no region marked
(whole-line-or-region-mode +1)
(eval-after-load "whole-line-or-region"
  '(diminish 'whole-line-or-region-mode))

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
  :bind ((:map emacs-lisp-mode-map
               ("C-c C-c" . eval-defun))))

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
    (bind-keys :map lispy-mode-map
               ("M-n" . sexy-next-paragraph)
               ("M-p" . sexy-prev-paragraph)
               ("<C-return>" . nil)
               ("C-," . nil)
               ("(" . sexy-parens)
               (")" . lispy-right)

               ("C-(" . lispy-parens-down)
               ("C-)" . lispy-out-forward-newline)

               ("n" . special-lispy-down)
               ("p" . special-lispy-up)

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
               ("k" . nil)
               ("j" . nil)
               ("<C-i>" . nil)


               ("[" . lispy-brackets)
               ("C-u" . undo-tree-undo)

               ("/" . special-lispy-splice)
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

               ("C-M-f" . sp-forward-sexp)
               ("C-M-b" . sp-backward-sexp)

               ("C-M-n" . sp-next-sexp)
               ("C-M-p" . sp-previous-sexp)

               ("C-S-f" . sp-forward-symbol)
               ("C-S-b" . sp-backward-symbol)

               ("C-<right>" . sp-forward-slurp-sexp)
               ("M-<right>" . sp-forward-barf-sexp)
               ("C-<left>" . sp-backward-slurp-sexp)
               ("M-<left>" . sp-backward-barf-sexp)

               ("C-M-t" . sp-transpose-sexp)
               ("C-M-w" . sp-copy-sexp)

               ("C-M-d" . delete-sexp)

               ("M-<backspace>" . nil)
               ("C-<backspace>" . nil)
               ("M-h" . sp-backward-kill-word)
               ([remap sp-backward-kill-word] . backward-kill-word)

               ("M-[" . sp-backward-unwrap-sexp)
               ("M-]" . sp-unwrap-sexp)

               ("C-x C-t" . sp-transpose-hybrid-sexp)

               ("]" . sp-rewrap-sexp))))

(use-package golden-ratio
  :ensure t
  :init
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
  :ensure t
  :mode (("\\.rkt\\'" . racket-mode))
  :config (progn
            (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
            (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)))

;; (use-package pdf-mode
;;   :load-path "site-lisp/pdf-mode.el/"
;;   :mode (("\\.rkt\\'" . pdf-mode)))

(use-package launch
  :ensure t
  :init
  (global-launch-mode +1))

(use-package reveal-in-osx-finder
  :ensure t
  :bind (("C-c f" . reveal-in-osx-finder)))

(require 'key-bindings)
(split-window-right)
