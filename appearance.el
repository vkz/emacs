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
                                   'face '(:foreground "#81a2be"))
                       "]"))))

;; TODO: should porbably move into a defun
(load-theme 'sanityinc-tomorrow-night t)

(provide 'appearance)
