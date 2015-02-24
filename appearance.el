(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t           ;???
      truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (tooltip-mode -1))

(toggle-frame-fullscreen)

;; Set custom theme path
(setq custom-theme-directory
      (expand-file-name "themes" user-emacs-directory))

;; TODO: customize theme, particularly color-faces in various helper
;; modes like helm-buffers. Use `helm-color' for such customizations.

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))


(setq my/default-font "-apple-Monaco-medium-*-regular-*-*-*-*-*-m-0-iso10646-1")
(setq my/cyrillic-font "-apple-Monaco-medium-*-regular-*-*-*-*-*-m-0-iso10646-1")
(set-fontset-font "fontset-default" 'cyrillic my/cyrillic-font)
(set-face-attribute 'default nil
                    :font my/default-font
                    :height 160
                    :weight 'normal
                    :foreground "#D7b78f"
                    :background "#161616")

;; Always have two windows in a frame
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (= (length (window-list)) 1)
              (split-window-right))))

;; (set-background-color "#161616")
(set-cursor-color "#40FF40")

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

;; don't wrap lines ever
(setq-default truncate-lines t)
(setq-default helm-truncate-lines t)

;; make the fringe (gutter) smaller
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Show current function in modeline.
(require 'which-func)
(which-function-mode)

;; Unclutter the modeline
(require 'diminish)

(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
(eval-after-load "helm-mode" '(diminish 'helm-mode))
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

;; TODO use `projectile-selected-face' to propertize that
;; TODO colors don't show up in the mode-line
(eval-after-load "projectile"
  '(setq projectile-mode-line
         '(:eval (list " [Pj:"
                       (propertize (projectile-project-name)
                                   'face 'projectile-selected-face)
                       "]"))))
(eval-after-load "projectile" '(diminish 'projectile-mode))

;; TODO can improve on that, maybe incorporate `projectile' current project
;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output
            (propertize (concat ".." output)
                        'face 'mode-line-folder-face)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-unmodified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-80col-face)

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
   (:eval (shorten-directory default-directory 15))
   mode-line-buffer-identification
   " %n "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))


;; TODO: should porbably move into a defun
(load-theme 'sanityinc-tomorrow-night t)

(provide 'appearance)
