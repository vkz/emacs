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
     dired+
     yasnippet
     guide-key
     highlight-escape-sequences
     whitespace-cleanup-mode
     elisp-slime-nav
     smooth-scrolling
     undo-tree
     shell-command
     helm
     helm-descbinds
     helm-c-yasnippet
     helm-swoop
     expand-region
     ;; smart-forward
     js2-refactor
     js2-mode
     easy-kill
     rainbow-mode
     diminish
     whole-line-or-region
     smartparens
     perspective
     persp-projectile
     helm-projectile
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

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

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; guide-key
;; TODO: missing support for js2-refactor prefix "C-c C-r" cause it's using two-key mnemonics
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-t r" "C-t 4" "C-t v" "C-c h" "C-c p" "C-t p" "C-c C-r" "<f1>"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'right)

;; Setup extensions
(require 'setup-magit)
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'shell '(require 'setup-shell)) ; TODO: helm support for completion
(require 'setup-hippie)                          ;TODO: practice all expansion methods
(require 'setup-yasnippet)              ; TODO: learn the power of snippets

;; NOTE `https://github.com/bbatsov/projectile/issues/586'
(require 'setup-perspective)

;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))

;; TODO: `semantic-mode' appears to work with `c' `cpp' `js' but not
;; `elisp'. What major modes does it support? What features does it actually provide?
(semantic-mode 1)

;; helm
(require 'setup-helm)

;; projectile
(require 'setup-projectile)

;; TODO: 'setup-eshell I do want it, but with shell-mode do I honestly need it?
;; TODO: 'setup-desktop to preserve emacs state between restarts
;; TODO: figure out TAGS (etags, ctags etc)
;; TODO: 'setup-bookmarks (install Bookmarks+)

;; TODO: 'setup-pop-win

;; language specific modes
;; TODO: 'setup-racket
;; TODO: 'setup-c
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(add-hook 'js-mode-hook (lambda () (custom-set-default 'js-indent-level 2)))
(setq programming-modes
      '(js2-mode js-mode c-mode c++-mode emacs-lisp-mode racket-mode))

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

(require 'expand-region)
(er/line-wise-select-advice)

;; (require 'smart-forward)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (elisp-slime-nav-mode t)
            (eldoc-mode 1)
            (rainbow-mode +1)))

;; TODO add whitespace auto cleanup on save (see bbatsov's prelude)
;; whitespace-mode config
;; TODO this should only be on in programming modes
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))
(global-whitespace-mode +1)

;; TODO only enable outside of `js2-mode', where Magnar's subst are more powerful.
;; Automatically insert matching braces and do other clever
;; things pertaining to braces and such.
;; (electric-pair-mode 1)

;; (require 'setup-smartparens)
(require 'smartparens-config)
(add-hook 'js-mode-hook 'turn-on-smartparens-mode)
(add-hook 'js-mode-hook 'show-smartparens-mode)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(require 'key-bindings)
(split-window-right)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; basic commands act on a whole line with no region marked
(whole-line-or-region-mode +1)

;; Does package really have anything to do with `require` though? I would've though that all it does having installed the package is add it's directory to the **load-path**. Having to `(require 'helm-config)` before installing the anything else seems really dissatisfying if only for the fact that Emacs doesn't make a good use of information that it already has. Also, having
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Experimenting with use-package
(eval-when-compile
  (require 'use-package))

(require 'bind-key)

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

;; TODO setup flycheck
;; -------------------
;; (use-package flycheck-ocaml             ; Check OCaml code with Merlin
;;   :ensure t
;;   :defer t
;;   :init (with-eval-after-load 'merlin
;;           (flycheck-ocaml-setup)))

;; Quick setup for EMACS
;; -------------------
;; Add opam emacs directory to your load-path by appending this to your .emacs:
;; ;; Add opam emacs directory to the load-path
;; (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
;; (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; ;; Load merlin-mode
;; (require 'merlin)
;; ;; Start merlin on ocaml files
;; (add-hook 'tuareg-mode-hook 'merlin-mode t)
;; (add-hook 'caml-mode-hook 'merlin-mode t)
;; ;; Enable auto-complete
;; (setq merlin-use-auto-complete-mode 'easy)
;; ;; Use opam switch to lookup ocamlmerlin binary
;; (setq merlin-command 'opam)

;; Take a look at https://github.com/the-lambda-church/merlin for more information

;; lispy.el
(eval-after-load "lispy" '(require 'setup-lispy))
