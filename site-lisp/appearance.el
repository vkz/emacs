(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t           ;???
      truncate-partial-width-windows nil)

;; Highlight current line
;; (global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
;; (show-paren-mode 1)

(when window-system
  (tooltip-mode -1))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-unmodified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-80col-face)

;; Basically just still the Spacemacs theming layer
(require 'theming)
(setq theming-modifications
      `((darktooth (default :inherit default :foreground "#D7b78f" :background "#161616")
                   (fringe :inherit fringe :background "#161616")
                   (mode-line :inherit mode-line :weight light :box (:line-width 1 :color "#969896") :height 100)
                   (mode-line-inactive :inherit mode-line-inactive :weight light :box (:line-width 1 :color "#373b41") :height 100)
                   (widget-button :inherit widget-button :weight light)
                   (mode-line-buffer-id :inherit mode-line-buffer-id :weight light)
                   (region :background "#8F621D" :inverse-video nil)
                   (which-func :foreground "#7f9f7f" :background nil :height 100)
                   ;; new
                   (mode-line-read-only-face :foreground "#4271ae")
                   (mode-line-modified-face :foreground "#c82829" :background "#ffffff")
                   (mode-line-unmodified-face :foreground "#969896")
                   (mode-line-folder-face :height 100)
                   (mode-line-position-face :foreground "#969896" :height 95)
                   (mode-line-80col-face :foreground "black" :background "#eab700"))
        (leuven (default :inherit default :foreground "#555" :background "#fdf6e3")
                (cursor :background "red")
                (hl-line :background "#E4E4E3")
                (avy-lead-face :weight light :foreground "white" :background "#e52b50")
                (show-paren-match :background "tan1")
                (font-lock-function-name-face :foreground "DodgerBlue3")
                (font-lock-variable-name-face :foreground "RoyalBlue3")
                (font-lock-keyword-face :foreground "DeepSkyBlue4")
                (font-lock-builtin-face :foreground "DeepSkyBlue4")
                (font-lock-constant-face :foreground "SeaGreen4")
                (font-lock-string-face :foreground "SeaGreen4")
                (font-lock-comment-face :foreground "tan3" :slant normal)
                (font-lock-comment-delimiter-face :foreground "tan3" :slant normal)
                (font-lock-type-face :foreground "Purple")
                (mode-line :inherit mode-line :weight light :box (:line-width 1 :color "#969896") :height 100)
                (mode-line-inactive :inherit mode-line-inactive :weight light :box (:line-width 1 :color "#373b41") :height 100)
                (widget-button :inherit widget-button :weight light)
                (mode-line-buffer-id :inherit mode-line-buffer-id :weight light)
                ;; new
                (mode-line-read-only-face :foreground "#4271ae")
                (mode-line-modified-face :foreground "#c82829" :background "#ffffff")
                (mode-line-unmodified-face :foreground "#969896")
                (mode-line-folder-face :height 100)
                (mode-line-position-face :foreground "#969896" :height 95)
                (mode-line-80col-face :foreground "black" :background "#eab700"))))

;; (toggle-frame-maximized)

;; Set custom theme path
;; (setq custom-theme-directory
;;       (expand-file-name "themes" user-emacs-directory))

;; (dolist
;;     (path (directory-files custom-theme-directory t "\\w+"))
;;   (when (file-directory-p path)
;;     (add-to-list 'custom-theme-load-path path)))

;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro"
;;                     :height 130
;;                     :weight 'light
;;                     :foreground "#D7b78f"
;;                     :background "#161616")

(use-package darktooth
  :ensure darktooth-theme
  :init
  (defun ze-load-dark-theme ()
    (mapcar #'disable-theme custom-enabled-themes)
    (set-face-attribute 'default nil
                        :family "Source Code Pro"
                        :height 130
                        :weight 'light
                        :foreground "#D7b78f"
                        :background "#161616")
    (load-theme 'darktooth 'no-confirm))
  (ze-load-dark-theme))

(use-package leuven
  :ensure leuven-theme
  :init
  (defun ze-load-light-theme ()
    (mapcar #'disable-theme custom-enabled-themes)
    (set-face-attribute 'default nil :family "Source Code Pro" :weight 'normal :height 130)
    (load-theme 'leuven 'no-confirm)))

(defun ze-toggle-theme ()
  "Toggle between dark and light themes."
  (interactive)
  (let ((dark-theme-p (eq (car custom-enabled-themes)
                          'darktooth)))
    (if dark-theme-p
        (ze-load-light-theme)
      (ze-load-dark-theme))))

(use-package color-theme-sanityinc-tomorrow
  :disabled t
  :ensure color-theme-sanityinc-tomorrow
  :init
  (set-face-attribute 'default nil :family "Source Code Pro" :weight 'normal :height 130)
  (call-interactively 'color-theme-sanityinc-tomorrow-day))

;; (use-package which-func                 ; Current function name
;;   :init (which-function-mode)
;;   :config
;;   (setq which-func-unknown "⊥" ; The default is really boring…
;;         which-func-format
;;         `((:propertize (" ➤ " which-func-current)
;;                        local-map ,which-func-keymap
;;                        face which-func
;;                        mouse-face mode-line-highlight
;;                        help-echo "mouse-1: go to beginning\n\
;; mouse-2: toggle rest visibility\n\
;; mouse-3: go to end"))))

;; (setq my/default-font "-apple-Monaco-medium-*-regular-*-*-*-*-*-m-0-iso10646-1")
;; (setq my/cyrillic-font "-apple-Monaco-medium-*-regular-*-*-*-*-*-m-0-iso10646-1")
;; (set-fontset-font "fontset-default" 'cyrillic my/cyrillic-font)

;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro"
;;                     :height 120
;;                     :weight 'light
;;                     :foreground "#D7b78f"
;;                     :background "#161616")

;; (set-face-attribute 'default nil
;;                     :family "Monaco"
;;                     :height 160
;;                     :weight 'medium
;;                     :foreground "#D7b78f"
;;                     :background "#161616")

;; Always have two windows in a frame
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (= (length (window-list)) 1)
              (split-window-right))
            (setq cursor-type 'box)
            ;; (set-cursor-color "SkyBlue2")
            ))

;; (set-background-color "#161616")

(defface font-lock-note-annotation
  '((t (:foreground "green")))
  "Face for the NOTE comment annotation ."
  :group 'comment-annotations)

(defface font-lock-study-annotation
  '((t (:inherit diary)))
  "Face for the STUDY comment annotation ."
  :group 'comment-annotations)

(defface font-lock-important-annotation
  '((t (:inherit diary)))
  "Face for the IMPORTANT comment annotation ."
  :group 'comment-annotations)

(defface font-lock-todo-annotation
  '((t (:foreground "red")))
  "Face for the TODO comment annotation ."
  :group 'comment-annotations)

(defface persp-selected-face
  '((t (:weight normal :height 100 :foreground "#81a2be")))
  ""
  :group 'mode-line)

(defface projectile-selected-face
  '((t (:inherit persp-selected-face)))
  ""
  :group 'mode-line)

;; don't wrap lines ever
(setq-default truncate-lines t)

;; make the fringe (gutter) smaller
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Unclutter the modeline
(require 'diminish)

(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
(eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "jslime" '(diminish 'jslime-mode))
(eval-after-load "whitespace" '(progn
                                 (diminish 'whitespace-mode)
                                 (diminish 'global-whitespace-mode)))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "js-mode" js-mode "JS")
(rename-modeline "lisp-mode" emacs-lisp-mode "El")
(rename-modeline "clojure-mode" clojure-mode "Clj")

;; TODO use `projectile-selected-face' to propertize that
;; TODO colors don't show up in the mode-line

(defface projectile-selected-face
  '((t (:inherit default)))
  "The face used to highlight the current projectile on the modeline.")

(eval-after-load "projectile"
  '(setq projectile-mode-line
         '(:eval (list " [Pj:"
                       (propertize (projectile-project-name)
                                   'face 'projectile-selected-face)
                       "]"))))
(eval-after-load "projectile" '(diminish 'projectile-mode))

;; TODO can improve on that, maybe incorporate `projectile' current project
;; Helper function
;; (defun shorten-directory (dir max-length)
;;   "Show up to `max-length' characters of a directory name `dir'."
;;   (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
;;         (output ""))
;;     (when (and path (equal "" (car path)))
;;       (setq path (cdr path)))
;;     (while (and path (< (length output) (- max-length 4)))
;;       (setq output (concat (car path) "/" output))
;;       (setq path (cdr path)))
;;     (when path
;;       (setq output
;;             (propertize (concat ".." output)
;;                         'face 'mode-line-folder-face)))
;;     output))

;; Mode line setup
(setq-default
 mode-line-format
 '(" "
   (-3 (:eval (propertize "%p" 'face
                          '(:inherit mode-line-position-face))))
   (:propertize " (" face mode-line-unmodified-face)
   "%l:"
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   (:propertize ")" face mode-line-unmodified-face)
   mode-line-client
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t (propertize " -- " 'face 'mode-line-unmodified-face))))
   ;; projectile-mode-line
   ;; (:eval (shorten-directory default-directory 15))
   ;; default-directory
   mode-line-buffer-identification
   " %n "
   mode-line-modes
   mode-line-misc-info
   (vc-mode vc-mode)
   mode-line-end-spaces))

(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode-enable)
  :init
  ;; TODO: in the newer version of the package 'rainbow-delimiters-mode-enable
  ;; might have been replaced with 'rainbow-delimiters-mode
  (dolist (hook '(prog-mode-hook))
    (add-hook hook 'rainbow-delimiters-mode-enable)))

;; TODO: should porbably move into a defun
;; (load-theme 'sanityinc-tomorrow-night t)
(toggle-frame-maximized)
(setq cursor-type 'box)
;; (set-cursor-color "SkyBlue2")
(provide 'appearance)
