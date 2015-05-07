;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defun ze/this-file ()
  "Return true path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))

;; Always use emacs.d of the current init.el
(setq user-emacs-directory (file-name-directory (file-truename (ze/this-file))))


;;; Requires

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(require 'rx)


;;; Initialisations and environment fixup
(setq inhibit-default-init t)

;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))

    (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH"))
      (add-to-list 'exec-path-from-shell-variables var))

    (exec-path-from-shell-initialize)
    (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.
    (with-eval-after-load 'info
      (dolist (dir (parse-colon-path (getenv "INFOPATH")))
        (when dir
          (add-to-list 'Info-directory-list dir))))))


;;; Customization interface
(defconst ze/custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (setq custom-file ze/custom-file
        custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load ze/custom-file 'no-error 'no-message))


;;; OS X support
(defconst on-mac (eq system-type 'darwin)
  "Are we on OSX?")

(use-package ns-win                     ; OS X window support
  :defer t
  :if on-mac
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                        ; workspace
        mac-command-modifier       'meta
        mac-right-command-modifier 'left
        ;; Let OS deal with other modifiers
        mac-option-modifier        'none
        mac-right-option-modifier  'none
        mac-function-modifier      'none))

(use-package ze-osx              ; Personal OS X tools
  :if on-mac
  :load-path "site-lisp/"
  :defines (ze/trash-tool)
  :config
  (if (executable-find ze/trash-tool)
      (defalias 'system-move-file-to-trash 'ze/move-file-to-trash)
    (warn "Trash support not available!
Install Trash from https://github.com/ali-rantakari/trash!
Homebrew: brew install trash")))


;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not on-mac) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      initial-scratch-message "Happy hacking!")
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

(use-package ze-scratch          ; My logo in the scratch buffer
  :commands (ze/insert-logo
             ze/insert-logo-into-scratch)
  :init (add-hook 'emacs-startup-hook #'ze/insert-logo-into-scratch))

(use-package sanityinc-tomorrow-night-theme                  ; My colour theme
  :load-path "themes/")

(use-package dynamic-fonts              ; Select best available font
  :ensure t
  :config
  (progn
    (setq
     dynamic-fonts-preferred-monospace-point-size (pcase system-type
                                                    (`darwin 16)
                                                    (_ 10))
     dynamic-fonts-preferred-proportional-fonts
     '(
       ;; http://www.google.com/get/noto/#/
       "Noto Sans"
       ;; https://www.mozilla.org/en-US/styleguide/products/firefox-os/typeface/
       "Fira Sans"
       ;; System fonts, as last resort
       "Helvetica"
       "Segoe UI"
       "DejaVu Sans"
       "Bitstream Vera"
       "Tahoma"
       "Verdana"
       "Arial Unicode MS"
       "Arial")
     dynamic-fonts-preferred-proportional-point-size (pcase system-type
                                                       (`darwin 16)
                                                       (_ 10)))
    (dynamic-fonts-setup)))

;; TODO `persistent-soft' won't work, without it font caching occurs every time
(use-package unicode-fonts              ; Map Unicode blocks to fonts
  :ensure t
  :disabled t
  :init
  (unicode-fonts-setup))



;; ;; Set up appearance early
;; (require 'appearance)

;; ;; Write backup files to own directory
;; (setq backup-directory-alist
;;       `(("." . ,(expand-file-name
;;                  (concat user-emacs-directory "backups")))))

;; ;; Make backups of files, even when they're in version control
;; (setq vc-make-backup-files t)

;; ;; Save point position between sessions
;; ;; `saveplace' is part of Emacs
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; ;; Are we on a mac?
;; (setq is-mac (equal system-type 'darwin))

;; ;; Lets start with a smattering of sanity
;; (require 'sane-defaults)

;; ;; Setup environment variables from the user's shell.
;; (when is-mac
;;   (require-package 'exec-path-from-shell)
;;   (exec-path-from-shell-initialize))

;; ;; guide-key
;; ;; TODO: missing support for js2-refactor prefix "C-c C-r" cause it's using two-key mnemonics
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("C-t r" "C-t 4" "C-t v" "C-c h" "C-c p" "C-t p" "C-c C-r" "<f1>"))
;; (guide-key-mode 1)
;; (setq guide-key/recursive-key-sequence-flag t)
;; (setq guide-key/popup-window-position 'right)

;; ;; Setup extensions
;; (require 'setup-magit)
;; (eval-after-load 'dired '(require 'setup-dired))
;; (eval-after-load 'shell '(require 'setup-shell)) ; TODO: helm support for completion
;; (require 'setup-hippie)                          ;TODO: practice all expansion methods
;; (require 'setup-yasnippet)              ; TODO: learn the power of snippets

;; ;; NOTE `https://github.com/bbatsov/projectile/issues/586'
;; (require 'setup-perspective)

;; ;; Font lock dash.el
;; (eval-after-load "dash" '(dash-enable-font-lock))

;; ;; TODO: `semantic-mode' appears to work with `c' `cpp' `js' but not
;; ;; `elisp'. What major modes does it support? What features does it actually provide?
;; (semantic-mode 1)

;; ;; helm
;; (require 'setup-helm)

;; ;; projectile
;; (require 'setup-projectile)

;; ;; TODO: 'setup-eshell I do want it, but with shell-mode do I honestly need it?
;; ;; TODO: 'setup-desktop to preserve emacs state between restarts
;; ;; TODO: figure out TAGS (etags, ctags etc)
;; ;; TODO: 'setup-bookmarks (install Bookmarks+)

;; ;; TODO: 'setup-pop-win

;; ;; language specific modes
;; ;; TODO: 'setup-racket
;; ;; TODO: 'setup-c
;; (eval-after-load 'js2-mode '(require 'setup-js2-mode))
;; (add-hook 'js-mode-hook (lambda () (custom-set-default 'js-indent-level 2)))
;; (setq programming-modes
;;       '(js2-mode js-mode c-mode c++-mode emacs-lisp-mode racket-mode))

;; ;; map files to modes
;; (require 'mode-mappings)

;; ;; highlight escape sequences, works only in javascript
;; (require 'highlight-escape-sequences)
;; (hes-mode)
;; (put 'font-lock-regexp-grouping-backslash 'face-alias 'js2-function-param)

;; ;; Functions (load all files in defuns-dir)
;; (setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
;; (dolist (file (directory-files defuns-dir t "\\w+"))
;;   (when (file-regular-p file)
;;     (load file)))

;; ;; Enable comment annotation keywords in programming modes
;; (comment-annotations-in-modes programming-modes)

;; (require 'expand-region)
;; (er/line-wise-select-advice)

;; ;; (require 'smart-forward)

;; ;; Elisp go-to-definition with M-. and back again with M-,
;; (autoload 'elisp-slime-nav-mode "elisp-slime-nav")
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (elisp-slime-nav-mode t)
;;             (eldoc-mode 1)
;;             (rainbow-mode +1)))

;; ;; TODO add whitespace auto cleanup on save (see bbatsov's prelude)
;; ;; whitespace-mode config
;; ;; TODO this should only be on in programming modes
;; (require 'whitespace)
;; (setq whitespace-line-column 80) ;; limit line length
;; (setq whitespace-style '(face tabs empty trailing lines-tail))
;; (global-whitespace-mode +1)

;; ;; TODO only enable outside of `js2-mode', where Magnar's subst are more powerful.
;; ;; Automatically insert matching braces and do other clever
;; ;; things pertaining to braces and such.
;; ;; (electric-pair-mode 1)

;; ;; (require 'setup-smartparens)
;; (require 'smartparens-config)
;; (add-hook 'js-mode-hook 'turn-on-smartparens-mode)
;; (add-hook 'js-mode-hook 'show-smartparens-mode)

;; ;; Emacs server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; (require 'key-bindings)
;; (split-window-right)

;; ;; revert buffers automatically when underlying files are changed externally
;; (global-auto-revert-mode t)

;; ;; basic commands act on a whole line with no region marked
;; (whole-line-or-region-mode +1)

;; ;; Does package really have anything to do with `require` though? I would've though that all it does having installed the package is add it's directory to the **load-path**. Having to `(require 'helm-config)` before installing the anything else seems really dissatisfying if only for the fact that Emacs doesn't make a good use of information that it already has. Also, having
;; (put 'narrow-to-page 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)

;; ;; Experimenting with use-package
;; (eval-when-compile
;;   (require 'use-package))

;; (require 'bind-key)

;; ;;; OCaml
;; ;; `https://github.com/realworldocaml/book/wiki/Installation-Instructions'
;; ;; brew install opam
;; ;; opam install core utop merlin ocp-indent
;; (use-package opam                       ; Initialize Emacs with OPAM env
;;   :ensure t
;;   :init (opam-init))

;; (use-package tuareg                     ; OCaml editing
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     ;; Disable SMIE indentation in Tuareg.  It's just broken currently…
;;     (setq tuareg-use-smie nil)

;;     ;; Please, Tuareg, don't kill my imenu
;;     (define-key tuareg-mode-map [?\C-c ?i] nil)

;;     ;; Also BACKSPACE, geeez
;;     (define-key tuareg-mode-map (kbd "<backspace>") nil)
;;     (define-key tuareg-mode-map (kbd "C-<backspace>") nil)

;;     ;; Gimme better indentation experience
;;     (use-package ocp-indent
;;       :ensure t)))

;; (use-package merlin                     ; Powerful Emacs backend for OCaml
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'tuareg-mode-hook #'merlin-mode)
;;   :config
;;   ;; Use Merlin from current OPAM env
;;   (setq merlin-command 'opam
;;         ;; Disable Merlin's own error checking in favour of Flycheck
;;         merlin-error-after-save nil))

;; (use-package utop
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'tuareg-mode-hook #'utop-minor-mode))

;; ;; TODO setup flycheck
;; ;; -------------------
;; ;; (use-package flycheck-ocaml             ; Check OCaml code with Merlin
;; ;;   :ensure t
;; ;;   :defer t
;; ;;   :init (with-eval-after-load 'merlin
;; ;;           (flycheck-ocaml-setup)))

;; ;; Quick setup for EMACS
;; ;; -------------------
;; ;; Add opam emacs directory to your load-path by appending this to your .emacs:
;; ;; ;; Add opam emacs directory to the load-path
;; ;; (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
;; ;; (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; ;; ;; Load merlin-mode
;; ;; (require 'merlin)
;; ;; ;; Start merlin on ocaml files
;; ;; (add-hook 'tuareg-mode-hook 'merlin-mode t)
;; ;; (add-hook 'caml-mode-hook 'merlin-mode t)
;; ;; ;; Enable auto-complete
;; ;; (setq merlin-use-auto-complete-mode 'easy)
;; ;; ;; Use opam switch to lookup ocamlmerlin binary
;; ;; (setq merlin-command 'opam)

;; ;; Take a look at https://github.com/the-lambda-church/merlin for more information

;; (use-package page-break-lines           ; Turn page breaks into lines
;;   :ensure t
;;   :init (global-page-break-lines-mode)
;;   :diminish page-break-lines-mode)

;; ;; (defun init--install-packages ()
;; ;;   (packages-install
;; ;;    '(use-package
;; ;;      dash
;; ;;      bash-completion
;; ;;      magit
;; ;;      dired+
;; ;;      yasnippet
;; ;;      guide-key
;; ;;      highlight-escape-sequences
;; ;;      whitespace-cleanup-mode
;; ;;      elisp-slime-nav
;; ;;      smooth-scrolling
;; ;;      undo-tree
;; ;;      shell-command
;; ;;      helm
;; ;;      helm-descbinds
;; ;;      helm-c-yasnippet
;; ;;      helm-swoop
;; ;;      expand-region
;; ;;      ;; smart-forward
;; ;;      js2-refactor
;; ;;      js2-mode
;; ;;      easy-kill
;; ;;      rainbow-mode
;; ;;      diminish
;; ;;      whole-line-or-region
;; ;;      smartparens
;; ;;      perspective
;; ;;      persp-projectile
;; ;;      helm-projectile
;; ;;      )))
