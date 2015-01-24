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

;; TODO: should porbably move into a defun
(load-theme 'sanityinc-tomorrow-night t)

(setq my/default-font "-apple-Monaco-medium-*-regular-*-*-*-*-*-m-0-iso10646-1")
(setq my/cyrillic-font "-apple-Monaco-medium-*-regular-*-*-*-*-*-m-0-iso10646-1")
(set-fontset-font "fontset-default" 'cyrillic my/cyrillic-font)
(set-face-attribute 'default nil
                    :font my/default-font
                    :height 160
                    :weight 'normal
                    :foreground "#D7b78f"
                    :background "#161616")

;; I want my setup to persist in new frames
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (split-window-right)))

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

(provide 'appearance)
