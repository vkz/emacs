(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t           ;???
      truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Set custom theme path
(setq custom-theme-directory
      (expand-file-name "themes" user-emacs-directory))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; TODO: should porbably move into a defun
(load-theme 'default-black t)


;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (tooltip-mode -1))

(provide 'appearance)
