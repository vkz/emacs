;;; init.el --- Emacs config of Vlad (zeRusski) Kozin -*- lexical-binding: t; -*-

;;; Commentary:

;; Borrowed heavily from Sebastian Wiesner
;; TODO borrow from magnars
;; TODO borrow from bbatsov
;; TODO borrow from oremacs
;;
;; User key prefixes:
;;
;; - C-c A: Align
;; - C-c a: Ag
;; - C-c b: Helm commands (b for "browse")
;; - C-c d: Data stuff
;; - C-c f: Files
;; - C-c h: Help and documentation
;; - C-c l: List things
;; - C-c m: Multiple cursors
;; - C-c s: Symbol commands

;; TODO make it a prefix, so guide-key can show hints
;; - C-c t: Toggle things and skeletons

;; - C-c u: Miscellaneous utilities
;; - C-c v: Version control
;; - C-c w: Web stuff

;; TODO modes requiring solid setup and use
;; * zop-to-char
;; * multiple-cursors
;; * smartparens
;; * avy (go-to-char and friends)
;; * buffer search with: isearch + swiper
;; * inter-file search with: ag, grep, wgrep, locate etc
;; * hippie-expand

;; TODO proper open-line stuff bound to C-o

;; TODO proglangs
;; * Racket
;; * JavaScript (browser and Node) / TypeScript
;; * OCaml
;; * Haskell


;; TODO learn vc / magit

;;; Code:

;;; Debugging


(setq message-log-max 10000)

;;; Bootstrap

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(defun ze/this-file ()
  "Return true path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))

;; Always use emacs.d of the current init.el
(setq user-emacs-directory (file-name-directory (file-truename (ze/this-file))))

;; Package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(require 'rx)

(use-package dash
  :ensure t
  :config
  (dash-enable-font-lock))

;;; zeBasics

(setq inhibit-default-init t)


;; OSX
(defconst on-mac (eq system-type 'darwin)
  "Are we on OSX?")

;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (and on-mac (display-graphic-p))
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

;; Customization interface
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

;; zeMinibuffer
(setq history-length 1000)              ; Store more history

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 180
                savehist-file (expand-file-name ".savehist" user-emacs-directory)))

(use-package desktop                    ; Save buffers, windows and frames
  :init (desktop-save-mode)
  :config (progn
            ;; Save desktops a minute after Emacs was idle.
            (setq desktop-auto-save-timeout 60)

            (dolist (mode '(magit-mode git-commit-mode))
              (add-to-list 'desktop-modes-not-to-save mode))))

;; TODO deserves a good binding and learn it
(use-package winner                     ; Undo and redo window configurations
  :init (winner-mode))

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

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete files to trash
(setq delete-by-moving-to-trash
      (or (not on-mac) ; Trash is well supported on other systems
          (fboundp 'system-move-file-to-trash)))

(use-package server                     ; The server of `emacsclient'
  :defer t
  :functions (server-running server-start)
  :init (with-eval-after-load 'server
          (unless (server-running-p)
            (server-start))))

;; this is XXI century PEOPLE ARE USING GUIs BY DEFAULT
;; take C-[ and C-i back and bind em usefully
;; `http://goo.gl/7Xmfn8'
;; TODO bind (kbd "C-<[>")
(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
(define-key input-decode-map [?\C-i] (kbd "<C-i>"))
(define-key input-decode-map [?\C-\S-i] (kbd "<C-I>"))

;; Homerow prefix
;; (define-key global-map (kbd "C-t") (lookup-key global-map (kbd "C-x")))
;; TODO better binding for "M-t"
;; TODO better binding for "M-u"
;; TODO better binding for "M-c"
;; TODO better binding for "M-."
;; TODO better binding for "C-,"
;; TODO better binding for "M-,"
(bind-keys* ("C-." . Control-X-prefix)  ;nice and symmetric to C-c
            ("C-t" . set-mark-command)  ;easy to press and follow with j* key-pairs
            ("<C-return>" . repeat))

(bind-keys

 ;; Don't kill Emacs that easily
 ("C-x r q" . save-buffers-kill-terminal)
 ("C-x C-c" . delete-frame)

 ;; Universal arg
 ;; TODO terrible binding?
 ("C-?" . universal-argument)
 ("C-!" . negative-argument)

 ;; Help
 ("<f1>" . help-command)
 ("<f1> h" . helm-apropos)

 ;; Switching frames, windows, buffers
 ("<backspace>" . other-window)
 ("M-<backspace>" . other-frame))

;;; zeLook

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not on-mac) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode 1)
(setq ring-bell-function #'ignore
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
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

;; Custom themes
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (expand-file-name "themes" user-emacs-directory))))

(use-package sanityinc-tomorrow-night-theme                ; My color theme
  :load-path "themes/"
  :defer t
  :init (load-theme 'sanityinc-tomorrow-night 'no-confirm)
  :config nil)

(use-package solarized
  :ensure solarized-theme
  :disabled t
  :defer t
  :init (load-theme 'solarized-dark 'no-confirm)
  :config nil)

;; Modeline
(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  :config
  (setq sml/theme 'powerline)
  (setq-default header-line-format
                '(which-func-mode ("" which-func-format " "))

                ;; Remove which func from the mode line, since we have it in the
                ;; header line
                mode-line-misc-info
                (assq-delete-all 'which-func-mode mode-line-misc-info)))

;; Standard stuff
(line-number-mode)
(column-number-mode)

;; TODO doesn't seem smart in some cases. Needs monitoring.
(use-package which-func                 ; Current function name in header line
  :init (which-function-mode)
  :config
  (setq which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))

(use-package dynamic-fonts              ; Select best available font
  :ensure t
  :config
  (progn
    (setq
     dynamic-fonts-preferred-monospace-fonts
     '(
       "Monaco"
       "Menlo"
       "Fira Mono"
       "DejaVu Sans Mono"
       "Inconsolata"
       "Consolas"
       "Bitstream Vera Mono"
       "Courier New")
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

;; don't wrap lines ever
(setq-default truncate-lines t)
(setq-default helm-truncate-lines t)

;; make the fringe (gutter) smaller
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Frames
(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package frame
  :bind (("C-c t F" . toggle-frame-fullscreen))
  :init (progn
          ;; Kill `suspend-frame'
          (global-set-key (kbd "C-z") nil)
          (global-set-key (kbd "C-x C-z") nil))
  :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq scroll-margin 0                   ; Drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

;; A function to disable highlighting of long lines in modes
(defun ze-whitespace-style-no-long-lines ()
  "Configure `whitespace-mode' for Org.

Disable the highlighting of overlong lines."
  (setq-local whitespace-style (-difference whitespace-style
                                            '(lines lines-tail))))

(defun ze-whitespace-mode-local ()
  "Enable `whitespace-mode' after local variables where set up."
  (add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

(use-package whitespace                 ; Highlight bad whitespace
  :bind (("C-c t w" . whitespace-mode))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'ze-whitespace-mode-local))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil)     ; Use `fill-column' for overlong lines
  :diminish whitespace-mode)

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

;; TODO is this good enough or should I be using smartparens or smth else?
(use-package paren                      ; Highlight paired delimiters
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

;;; zeBuffers

(use-package ze-buffers          ; Personal buffer tools
  :load-path "site-lisp/"
  :commands (ze-force-save-some-buffers
             ze-do-not-kill-important-buffers)
  :init (progn
          (add-hook 'kill-buffer-query-functions
                    #'ze-do-not-kill-important-buffers)

          ;; Autosave buffers when focus is lost, see
          ;; http://emacsredux.com/blog/2014/03/22/a-peek-at-emacs-24-dot-4-focus-hooks/
          (add-hook 'focus-out-hook #'ze-force-save-some-buffers)))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :config (setq ibuffer-formats
                '((mark modified read-only vc-status-mini " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " "
                        (vc-status 16 16 :left)
                        " "
                        filename-and-process)
                  (mark modified read-only " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " " filename-and-process)
                  (mark " "
                        (name 16 -1)
                        " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

;; TODO appears to be OFF by default
(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :defer t)

;;; zeFiles

(use-package files
  :bind (("C-c f v" . revert-buffer))
  :config
  ;; Use GNU ls for Emacs
  (-when-let (gnu-ls (and on-mac (executable-find "gls")))
    (setq insert-directory-program gnu-ls)))

(use-package tramp                      ; Access remote files
  :defer t
  :config
  ;; Store auto-save files locally
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (progn
    (require 'dired-x)

    (setq dired-auto-revert-buffer t    ; Revert on re-visiting
          ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
          ;; uses human-readable sizes, and `-F' appends file-type classifiers
          ;; to file names (for better highlighting)
          dired-listing-switches "-alhF"
          dired-ls-F-marks-symlinks t   ; -F marks links with @
          ;; Inhibit prompts for simple recursive operations
          dired-recursive-copies 'always)

    (when (or (memq system-type '(gnu gnu/linux))
              (string= (file-name-nondirectory insert-directory-program) "gls"))
      ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
      ;; `--group-directories-first' lists directories before files, and `-v'
      ;; sorts numbers in file names naturally, i.e. "image1" goes before
      ;; "image02"
      (setq dired-listing-switches
            (concat dired-listing-switches " --group-directories-first -v")))))

;; TODO bind "C-x C-d" to something more useful than list-directory
(use-package dired-x                    ; Additional tools for Dired
  :bind (("C-x D" . dired-jump)
         ("C-x d" . dired-jump-other-window))
  :config
  (progn
    (setq dired-omit-verbose nil)        ; Shut up, dired

    (when on-mac
      ;; OS X bsdtar is mostly compatible with GNU Tar
      (setq dired-guess-shell-gnutar "tar"))))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :init (ignoramus-setup))

(use-package hardhat                    ; Protect user-writable files
  :ensure t
  :init (global-hardhat-mode))

(use-package recentf                    ; Save recently visited files
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'" ; Package files
                              "/node_modules/.*\\'"
                              "/itsalltext/" ; It's all text temp files
                              ;; And all other kinds of boring files
                              #'ignoramus-boring-p)
        recentf-save-file (expand-file-name ".recentf" user-emacs-directory)))

(use-package saveplace                  ; Save point position in files
  :config (setq-default save-place t
                        save-place-file (expand-file-name ".places" user-emacs-directory)))

(setq view-read-only t)                 ; View read-only files

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode))

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package launch                     ; Open files in external programs
  :ensure t
  :init (global-launch-mode))

;;; zeEditor

;; Disable tabs, but give them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)
;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; TODO elec-pair is busted and needs to be replaced
;; * replace with custom pairing in js2-mode
;; * smartparens elsewhere
(use-package elec-pair                  ; Electric pairs
  :init (electric-pair-mode))

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
;; TODO reasonable on my Air is around 65
(setq-default fill-column 85)
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

(use-package subword                    ; Subword/superword editing
  :defer t
  :diminish subword-mode)

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c r" . vr/query-replace)
         ("C-c R" . vr/replace)))

(use-package browse-kill-ring           ; Browse kill ring interactively
  :ensure t
  :bind (("C-c y" . browse-kill-ring)))

;; TODO deserves better binding! Versatile enough to completely replace M-d[h]
;; bindings. Create a Hydra for it.
(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

;; TODO try actually using it!
(use-package easy-kill                  ; Easy killing and marking on C-w
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

;; TODO better binding?
(use-package align                      ; Align text in buffers
  :bind (("C-c A a" . align)
         ("C-c A c" . align-current)
         ("C-c A r" . align-regexp)))

;; TODO deserves easier binding, particularly mark-more-like-this
(use-package multiple-cursors           ; Edit text with multiple cursors
  :ensure t
  :bind (("C-c m e"   . mc/mark-more-like-this-extended)
         ("C-c m h"   . mc/mark-all-like-this-dwim)
         ("C-c m l"   . mc/edit-lines)
         ("C-c m n"   . mc/mark-next-like-this)
         ("C-c m p"   . mc/mark-previous-like-this)
         ("C-c m r"   . vr/mc-mark)
         ("C-c m C-a" . mc/edit-beginnings-of-lines)
         ("C-c m C-e" . mc/edit-ends-of-lines)
         ("C-c m C-s" . mc/mark-all-in-region))
  :config
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("M-=" . ze-mark-paragraph)))

(use-package undo-tree                  ; Branching undo
  :ensure t
  :init (progn
          (global-undo-tree-mode)
          (unbind-key "C-/" undo-tree-map)
          (unbind-key "C-?" undo-tree-map)
          (unbind-key "C-_" undo-tree-map))
  :bind (("C-u"   . undo-tree-undo)
         ("M-u"   . undo-tree-redo)
         ("C-M-u" . undo-tree-visualize))
  :diminish undo-tree-mode)

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Other key-bindings for editing

;; TODO great one, needs better binding
(bind-key [remap just-one-space] #'cycle-spacing)

;;; zeNav

(use-package avy
  :load-path "site-lisp/avy/"
  :defer t
  :commands (avy--regex-candidates)     ;HACK swiper needs it
  :init (setq avy-styles-alist '((avy-goto-char-2 . post)
                                 (avy-goto-char   . post))
              avy-background t
              avy-all-windows nil)
  ;; TODO need better bindings, better yet j- bi-sequences like jw, jl, jc
  :bind (("M-'"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :config (avy-setup-default))

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(use-package imenu-anywhere             ; IDO-based imenu across open buffers
  :disabled t
  :ensure t
  :bind (("C-c i" . helm-imenu-anywhere)))

;; Search
(use-package isearch                    ; Search buffers
  :bind (("C-c s s" . isearch-forward-symbol-at-point)))

(use-package swiper
  :ensure t
  :defer t
  :init (setq ivy-use-virtual-buffers t)
  :config (ivy-mode 1)
  :bind (("C-s"     . swiper)
         ("C-r"     . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)))

(use-package locate                     ; Search files on the system
  :defer t
  :config
  ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
  (-when-let (mdfind (and on-mac (executable-find "mdfind")))
    (setq locate-command mdfind)))

(use-package ag                         ; Search code in files/projects
  :ensure t
  :bind (("C-c a d" . ag-dired-regexp)
         ("C-c a D" . ag-dired)
         ("C-c a f" . ag-files)
         ("C-c a k" . ag-kill-other-buffers)
         ("C-c a K" . ag-kill-buffers))
  :config
  (setq ag-reuse-buffers t            ; Don't spam buffer list with ag buffers
        ag-highlight-search t         ; A little fanciness
        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))

(use-package wgrep                      ; Edit grep/occur/ag results in-place
  :ensure t
  :defer t)

(use-package wgrep-ag                   ; Wgrep for ag
  :ensure t
  :defer t)

;;; zeHelm

;; TODO work through the tutorial again
;; TODO choose stuff I actually need and bind it appropriately
(use-package helm
  :ensure t
  :bind (
         ;; Replace some standard bindings with Helm equivalents
         ("M-s o"     . helm-occur)
         ("M-x"       . helm-M-x)
         ("C-x C-m"   . helm-M-x)
         ("M-y"       . helm-show-kill-ring)
         ("C-x r i"   . helm-register)
         ("C-x b"     . helm-mini)
         ;; Special helm bindings
         ("C-c b b"   . helm-resume)
         ("C-c b C"   . helm-colors)
         ("C-c b *"   . helm-calcul-expression)
         ("C-c b 8"   . helm-ucs)
         ("C-c b M-:" . helm-eval-expression-with-eldoc)
         ;; Helm features in other maps
         ("C-c i"     . helm-semantic-or-imenu)
         ("C-c h a"   . helm-apropos)
         ("C-c h e"   . helm-info-emacs)
         ("C-c h i"   . helm-info-at-point)
         ("C-c h m"   . helm-man-woman)
         ("C-c f r"   . helm-recentf)
         ("C-c f f"   . helm-find-files)
         ("C-c f l"   . helm-locate-library)
         )
  :init (progn
          ;; Shut up, f****** `helm-config'
          (defvar helm-command-prefix-key)
          (setq helm-command-prefix-key nil)

          (helm-mode 1))
  :config (setq helm-split-window-in-side-p t))

(use-package helm-files
  :ensure helm
  :defer t
  :config (setq helm-recentf-fuzzy-match t
                ;; Use recentf to find recent files
                helm-ff-file-name-history-use-recentf t
                ;; Find library from `require', `declare-function' and friends
                helm-ff-search-library-in-sexp t))

;; TODO fix how helm-buffers look! Smaller face, no stupid size column, fix path
;; column, or rid of it: make it freaking helpful.
(use-package helm-buffers
  :ensure helm
  :defer t
  :config (setq helm-buffers-fuzzy-matching t))

(use-package helm-ag
  :ensure t
  :bind (("C-c a a" . helm-do-ag)
         ("C-c a A" . helm-ag))
  :config (setq helm-ag-fuzzy-match t))

;;; zeComplete

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

(use-package hippie-exp                 ; Powerful expansion and completion
  :defer t
  :config (setq hippie-expand-try-functions-list
                '(try-expand-dabbrev-closest-first
                  try-complete-file-name
                  try-expand-dabbrev-all-buffers
                  try-expand-dabbrev-from-kill
                  try-expand-all-abbrevs
                  try-complete-lisp-symbol-partially
                  try-complete-lisp-symbol
                  try-complete-lisp-symbol-without-namespace)))

(use-package ze-hippie       ; Custom expansion functions
  :load-path "site-lisp/"
  :bind (("<C-i>" . hippie-expand-no-case-fold)
         ("<C-I>" . hippie-expand-lines))
  :defines (he-search-loc-backward
            he-search-loc-forward)
  :commands (try-complete-lisp-symbol-without-namespace
             try-expand-dabbrev-closest-first
             try-expand-line-closest-first
             hippie-expand-lines
             hippie-expand-no-case-fold))

;; TODO try `ivy' as completion backend
(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :init (global-company-mode)
  :config
  (progn
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t))
  :diminish company-mode)

(use-package company-math               ; Completion for Math symbols
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          ;; Add backends for math characters
          (add-to-list 'company-backends 'company-math-symbols-unicode)
          (add-to-list 'company-backends 'company-math-symbols-latex)))

(use-package helm-company
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          ;; Use Company for completion
          (bind-key [remap completion-at-point] #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-active-map)))

;;; zeProg

(use-package compile                    ; Compile from Emacs
  :bind (("C-c c" . compile)
         ("C-c C" . recompile))
  :config (progn
            (setq compilation-ask-about-save nil
                  ;; Kill old compilation processes before starting new ones,
                  ;; and automatically scroll up to the first error.
                  compilation-scroll-output 'first-error)

            (add-to-list 'display-buffer-alist
                         `(,(rx bos "*compilation")
                           (display-buffer-reuse-window
                            display-buffer-in-side-window)
                           (side            . bottom)
                           (reusable-frames . visible)
                           (window-height   . 0.4)))))

(use-package ze-compile          ; Personal helpers for compilation
  :load-path "site-lisp/"
  :commands (ze-colorize-compilation-buffer)
  ;; Colorize output of Compilation Mode, see
  ;; http://stackoverflow.com/a/3072831/355252
  :init (add-hook 'compilation-filter-hook
                  #'ze-colorize-compilation-buffer))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package rainbow-mode               ; Fontify color values in code
  :ensure t
  :bind (("C-c t r" . rainbow-mode))
  :config (dolist (hook '(css-mode-hook emacs-lisp-mode-hook))
            (add-hook hook #'rainbow-mode)))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s o" . highlight-symbol-occur)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init (progn (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
               (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)

(use-package eldoc                      ; Documentation in minibuffer
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :config
  (setq-default eldoc-documentation-function #'describe-char-eldoc))

;;; zeLisp


;; TODO try `Lispy' first

;;; zeElisp

(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

;; TODO experiment with it
(use-package pcre2el                    ; Convert regexps to RX and back
  :disabled t
  :ensure t
  :init (rxt-global-mode))

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :defer t
  :init (with-eval-after-load 'lisp-mode
          (bind-key "C-c e" #'macrostep-expand emacs-lisp-mode-map)
          (bind-key "C-c e" #'macrostep-expand lisp-interaction-mode-map)))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c u z" . ielm)))

(use-package lisp-mode                  ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config
  (progn
    (require 'ert)))

(use-package ze-lisp             ; Personal tools for Emacs Lisp
  :load-path "site-lisp/"
  :commands (ze-find-cask-file
             ze-add-use-package-to-imenu)
  :init (progn
          (add-hook 'emacs-lisp-mode-hook #'ze-add-use-package-to-imenu)

          (with-eval-after-load 'lisp-mode
            (bind-key "C-c f c" #'ze-find-cask-file
                      emacs-lisp-mode-map))))

(bind-key "C-c t d" #'toggle-debug-on-error)

;;; zeRacket


;;; zeScala


;;; zeHaskell


;;; zeOCaml

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

(use-package flycheck-ocaml             ; Check OCaml code with Merlin
  :ensure t
  :defer t
  :init (with-eval-after-load 'merlin
          (flycheck-ocaml-setup)))

;;; zeJS and zeWebs

(use-package web-mode                   ; Template editing
  :ensure t
  :defer t
  :config
  (setq web-mode-markup-indent-offset 2))

;; TODO add js customizations from `setup-js2-mode.el', needs review and
;; trimming
(use-package js2-mode                   ; Javascript editing
  :ensure t
  :mode "\\.js$"
  :init
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  :config (progn
            (setq-default js2-basic-offset 2)))

(use-package css-mode
  :defer t
  :config
  (progn
    ;; Run Prog Mode hooks, because for whatever reason CSS Mode derives from
    ;; `fundamental-mode'.
    (add-hook 'css-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

    ;; Mark css-indent-offset as safe local variable.  TODO: Report upstream
    (put 'css-indent-offset 'safe-local-variable #'integerp)))

(use-package css-eldoc                  ; Basic Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :defines (css-eldoc-hash-table)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

;;; zeVC

(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init (progn
          ;; Highlight changes to the current file in the fringe
          (global-diff-hl-mode)
          ;; Highlight changed files in the fringe of Dired
          (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

          ;; Fall back to the display margin, if the fringe is unavailable
          (unless (display-graphic-p)
            (diff-hl-margin-mode))))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x v g" . magit-status)
         ("C-x v v" . magit-status)
         ("C-x v g" . magit-blame-mode)
         ("C-x v l" . magit-file-log))
  :init
  ;; Seriously, Magit?! Set this variable before Magit is loaded to silence the
  ;; most stupid warning ever
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    ;; Shut up, Magit!
    (setq magit-save-some-buffers 'dontask
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          ;; Except when you ask something useful…
          magit-set-upstream-on-push t)

    ;; Set Magit's repo dirs for `magit-status' from Projectile's known
    ;; projects.  Initialize the `magit-repo-dirs' immediately after Projectile
    ;; was loaded, and update it every time we switched projects, because the
    ;; new project might have been unknown before
    (defun ze-magit-set-repo-dirs-from-projectile ()
      "Set `magit-repo-dirs' from known Projectile projects."
      (let ((project-dirs (bound-and-true-p projectile-known-projects)))
        ;; Remove trailing slashes from project directories, because Magit adds
        ;; trailing slashes again, which breaks the presentation in the Magit
        ;; prompt.
        (setq magit-repo-dirs (mapcar #'directory-file-name project-dirs))))

    (with-eval-after-load 'projectile
      (ze-magit-set-repo-dirs-from-projectile))

    (add-hook 'projectile-switch-project-hook
              #'ze-magit-set-repo-dirs-from-projectile))

  :diminish magit-auto-revert-mode)

(use-package magit-gh-pulls
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package git-commit-mode            ; Git commit message mode
  :ensure t
  :defer t)

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-rebase-mode            ; Mode for git rebase -i
  :ensure t
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind (("C-c v t" . git-timemachine)))

;;; zeProjects

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config
  (progn
    ;; Remove dead projects when Emacs is idle
    (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

    (setq projectile-completion-system 'helm
          projectile-buffers-filter-function
          ;; Include buffers with files, processes or Dired
          #'(lambda (buffers)
              (seq-filter (lambda (buffer)
                            (or (buffer-file-name buffer)
                                (get-buffer-process buffer)
                                (with-current-buffer buffer
                                  (derived-mode-p 'dired-mode))))
                          buffers))
          projectile-find-dir-includes-top-level t
          projectile-mode-line '(:propertize
                                 (:eval (concat " " (projectile-project-name)))
                                 face font-lock-constant-face)
          projectile-cache-file (expand-file-name ".projectile.cache" user-emacs-directory)
          projectile-known-projects-file (expand-file-name ".projectile-bookmarks.eld" user-emacs-directory)))
  :diminish projectile-mode)

(use-package helm-projectile
  :ensure t
  :defer t
  :init (with-eval-after-load 'projectile (helm-projectile-on))
  :config (setq projectile-switch-project-action #'helm-projectile))

;;; zeUtils

(use-package json-reformat              ; Reformat JSON
  :ensure t
  :defer t
  :bind (("C-c u j" . json-reformat-region)))

(use-package bug-reference              ; Turn bug references into buttons
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
               (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c l p" . paradox-list-packages)
         ("C-c l P" . package-list-packages-no-fetch))
  :config
  ;; Don't ask for a token, please, and don't bug me about asynchronous updates
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

(use-package proced                     ; Edit system processes
  ;; Proced isn't available on OS X
  :if (not on-mac)
  :bind ("C-x p" . proced))

(use-package calendar                   ; Built-in calendar
  :bind ("C-c u c" . calendar)
  :config
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))

(use-package time                       ; Show current time
  :bind (("C-c u i" . emacs-init-time)
         ("C-c u T" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("Europe/Berlin"    "Berlin")
                                  ("Europe/London"    "London")
                                  ("Europe/Istanbul"  "Istanbul")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ("America/New_York" "New York (USA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)"))))

(use-package bug-hunter                 ; Search init file for bugs
  :ensure t)

;; Terminal emulation and shells
(use-package shell                      ; Dumb shell in Emacs
  :bind ("C-c j" . start-or-switch-to-shell)
  :config (add-to-list 'display-buffer-alist
                       `(,(rx bos "*shell")
                         (display-buffer-reuse-window
                          display-buffer-in-side-window
                          (side            . bottom)
                          (reusable-frames . visible)
                          (window-height   . 0.4)))))

(use-package term                       ; Terminal emulator in Emacs
  :bind ("C-c u S" . ansi-term))

;; Net & Web
(use-package browse-url                 ; Browse URLs
  :bind (("C-c w u" . browse-url)))

(use-package eww                        ; Emacs' built-in web browser
  :bind (("C-c w b" . eww-list-bookmarks)
         ("C-c w w" . eww)))

(use-package sx                         ; StackExchange client for Emacs
  :ensure t
  :bind (("C-c w s" . sx-tab-frontpage)
         ("C-c w S" . sx-tab-newest)
         ("C-c w a" . sx-ask)))

(use-package sx-compose
  :ensure sx
  :defer t
  :config
  (progn
    ;; Don't fill in SX questions/answers, and use visual lines instead.  Plays
    ;; more nicely with the website.
    (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
    (add-hook 'sx-compose-mode-hook #'visual-line-mode)
    (add-hook 'sx-compose-mode-hook
              #'ze-whitespace-style-no-long-lines)

    ;; Clean up whitespace before sending questions
    (add-hook 'sx-compose-before-send-hook
              (lambda ()
                (whitespace-cleanup)
                t))

    (bind-key "M-q" #'ignore sx-compose-mode-map)))

(use-package sx-question-mode
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

(use-package erc                        ; Powerful IRC client
  :defer t
  :config
  (progn
    ;; Default server and nick
    (setq erc-server "chat.freenode.net"
          erc-port 7000
          erc-nick "zeRusski"
          erc-nick-uniquifier "_"
          ;; Never open unencrypted ERC connections
          erc-server-connect-function 'erc-open-tls-stream)

    ;; Spell-check ERC buffers
    (add-to-list 'erc-modules 'spelling)
    (erc-update-modules))

  ;; Ignore useless messages
  (add-hook 'erc-mode-hook
            '(lambda ()
               (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
                     erc-user-full-name "Vlad Kozin"))))

(use-package erc-join                   ; Automatically join channels with ERC
  :defer t
  :config
  ;; Standard channels on Freenode
  (setq erc-autojoin-channels-alist
        '(("\\.freenode\\.net" . ("#racket" "#ocaml" "#haskell-beginners")))))

(use-package erc-track                  ; Track status of ERC in mode line
  :defer t
  :config
  ;; Switch to newest buffer by default, and don't ask before rebinding the keys
  (setq erc-track-switch-direction 'newest
        erc-track-enable-keybindings t))

;;; zeHelp

(use-package find-func                  ; Find function/variable definitions
  :bind (("C-x F"   . find-function)
         ("C-x 4 F" . find-function-other-window)
         ("C-x K"   . find-function-on-key)
         ("C-x V"   . find-variable)
         ("C-x 4 V" . find-variable-other-window)))

(use-package info                       ; Info manual viewer
  :defer t)

(use-package helm-descbinds
  :ensure t
  :defer t
  :init (helm-descbinds-mode))

(use-package dash-at-point
  :ensure t
  :defer t
  :bind (("C-c h d" . dash-at-point)
         ("C-c h D" . dash-at-point-with-docset)))

(bind-key "C-c h b" #'describe-personal-keybindings)

(use-package ediff-wind
  :defer t
  :config
  ;; Prevent Ediff from spamming the frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

;; TODO really needs auto-fill mode wrapping around 80 or so
(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :bind (("C-c t R" . writeroom-mode)))

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c l b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config (setq bookmark-save-flag 1))

;;; zeHelpers

(use-package ze-misc
  :load-path "site-lisp/"
  :defer t
  :commands (ze-auto-fill-comments-mode)
  :bind
  (("C-x 3"                        . ze-split-window-right)
   ([remap move-beginning-of-line] . ze-bol)
   ("C-o"                          . ze-smart-open-line)
   ("C-c u d"                      . ze-insert-current-date)
   ("C-c c"                        . ze-toggle-comment)
   ("C-c d"                        . ze-dup)
   ("C-c M-d"                      . ze-dup-and-comment)
   ("C-<backspace>"                . ze-buffer-behind)
   ("C-c <tab>"                    . ze-swap-windows)
   ("C-c <backspace>"              . ze-other-window)
   ("<escape>"                     . ze-bury-buffer-then-switch)
   ("S-<escape>"                   . bury-buffer))
  :init (add-hook 'prog-mode-hook #'ze-auto-fill-comments-mode))

(use-package ze-snippet-helpers
  :load-path "site-lisp/"
  :demand t)

;;; zeBindings


;; Killing stuff
;; TODO install `whole-line-or-region'
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "C-w") 'kill-region)
(global-set-key (kbd "M-h") 'kill-region-or-backward-word)
(global-set-key [remap kill-ring-save] 'easy-kill) ;M-w



;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
