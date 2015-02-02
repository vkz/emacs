;; Load Perspective
(require 'perspective)

;; TODO: doesn't seem to play nicely with projectile
;; `https://github.com/bbatsov/projectile/issues/586'
;; avoid using them together for now

;; TODO: unfortunately prespectives aren't persistent between emacs
;; sessions, so there's no need to recreate the state between emacs
;; restarts `https://github.com/nex3/perspective-el/issues/19' Using
;; `desktop' could be a temporary solution that restores the state but
;; doesn't keep track of perspectives. Sounds like something that
;; shouldn't be too tricky to fix.

(persp-mode t)

;; Change prefix to `C-t p' which is more natural in Dvorak

;; VERSION-1 leaves doesn't touch the default setup
;; Surprisingly `guide-key' picks it up correctly
(define-key persp-mode-map (kbd "C-t p") (lookup-key persp-mode-map (kbd "C-x x")))

;; VERSION-2 cleares all default bindings introducing a new sparse-map
;; for the prefix

;; (eval-after-load 'perspective
;;   '(progn
;;      (define-key persp-mode-map (kbd "C-x x") nil)
;;      (define-key persp-mode-map (kbd "C-x x s") nil)
;;      (define-key persp-mode-map (kbd "C-x x k") nil)
;;      (define-key persp-mode-map (kbd "C-x x c") nil)
;;      (define-key persp-mode-map (kbd "C-x x r") nil)
;;      (define-key persp-mode-map (kbd "C-x x a") nil)
;;      (define-key persp-mode-map (kbd "C-x x i") nil)
;;
;;      ;; Make a fresh prefix sparse keymap and bind it to `C-t p'
;;      (define-prefix-command 'perspective)
;;      (define-key persp-mode-map (kbd "C-t p") 'perspective)
;;
;;      ;; Populate the keymap
;;      (define-key persp-mode-map (kbd "C-t p s") 'persp-switch)
;;      (define-key persp-mode-map (kbd "C-t p k") 'persp-remove-buffer)
;;      (define-key persp-mode-map (kbd "C-t p c") 'persp-kill)
;;      (define-key persp-mode-map (kbd "C-t p r") 'persp-rename)
;;      (define-key persp-mode-map (kbd "C-t p a") 'persp-add-buffer)
;;      (define-key persp-mode-map (kbd "C-t p i") 'persp-import)))

;; Always have two windows damn it!
(add-hook 'persp-switch-hook
          (lambda ()
            (when (= (length (window-list)) 1)
              (with-selected-window (split-window-right)))))
;; (setq persp-switch-hook (cdr persp-switch-hook))

;; TODO make `persp-next' cycle through all perspectives instead of stupidly
;; stopping at the end

;; TODO process-per-perspective so that each persp has its own *shell*, *term*,
;; *eshell*, etc. Easy enough to do. Just keep track of all processes in the
;; persp and rename them on persp-switch: [p1] has *shell* and is active, [p2]
;; has *shell-p2*, now (persp-switch "p2") renames *shell* to *shell-p1* and
;; *shell-p2* to *shell*. Belongs in `perspective.el' not here of course.

(provide 'setup-perspective)
