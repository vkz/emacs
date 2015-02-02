;; What do I want from my initial Emacs setup?
;; -------------------------------------------
;; * simple, helpful, unobtrusive appearance that doesn't strain my eyes
;; -- fontlocking for specific languages
;; -- fontlocking for filesystem units (file types, directories, etc)
;; -- uncluttered informative minibuffer mode-line
;; * comfortable default bindings to navigate text in a buffer
;; * comfortable default bindings to navigate files, buffers, projects, windows, frames
;; * simple way to grep files, buffers, projects and navigate results
;; * shell where I can move around with Emacs keybindings
;; * multiple shell-instances and easy way to switch between them
;; * powerful yet visual filesystem management
;; * get Emacs help and discover functionality with minimum keystrokes and cognitive load
;; * any aux/help buffers should disappear on losing focus without my switching to them
;; * automatically recover Emacs state between editor restarts
;; * straightforward version control
;; * JavaScript, Racket, C support:
;; -- quick way to navigate potentially large projects
;; -- semantics-aware syntax highlighting
;; -- jump-to-definition (cross module boundaries, with sys libraries) at point and on demand
;; -- documentation lookup for symbol at point (at least function arguments)
;; -- comfortable bindings to navigate semantic units (defuns, blocks, etc) in a buffer
;; -- compile-build-run current buffer (file, module, project)
;; -- jump-to-error when compilation fails
;; -- semantics-aware refactoring (rename var, etc)
;; * All this should be OS agnostic and just work on Mac, Win, Nix

;; Temp fix! Remove!
;; (setq user-emacs-directory
;;       "/Users/kozin/.personal_configs/emacs.candidate")
(setq user-emacs-directory "/Users/kozin/.emacs.d")

;; -------------------------------------------------

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set up load path
(add-to-list 'load-path user-emacs-directory)

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

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(bash-completion
     magit
     dired+
     yasnippet
     guide-key
     nodejs-repl
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
     projectile
     helm-projectile
     persp-projectile
     expand-region
     smart-forward
     js2-refactor
     js2-mode
     easy-kill
     rainbow-mode
     diminish
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

;; TODO: perspectives don't persist between emacs restarts
;; TODO: doesn't play nicely with projectile. Avoid using it for now.
;; see `https://github.com/bbatsov/projectile/issues/586'
;; (require 'setup-perspective)

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
(setq programming-modes '(js2-mode c-mode c++-mode emacs-lisp-mode racket-mode))

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
(require 'smart-forward)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (elisp-slime-nav-mode t)
            (eldoc-mode 1)
            (rainbow-mode +1)))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(require 'key-bindings)
(split-window-right)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Does package really have anything to do with `require` though? I would've though that all it does having installed the package is add it's directory to the **load-path**. Having to `(require 'helm-config)` before installing the anything else seems really dissatisfying if only for the fact that Emacs doesn't make a good use of information that it already has. Also, having
