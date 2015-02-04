;; projectile
(require 'projectile)
(projectile-global-mode)
(unless (equal system-type 'windows-nt)
  (setq projectile-indexing-method 'alien))
(setq projectile-completion-system 'helm)

;; enable caching
;; `C-c p i'  or `C-u' before standard commands to invalidate cache
(setq projectile-enable-caching t)

;; helm-projectile
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

;; TODO: rebind `find-file-other-window' to a single stroke key probably in
;; `helm-projectile-find-file-map'. Fair chance that this shouldn't be
;; done directly but rather via `helm-projectile-file-actions'
;; otherwise it'll only work for finding files?

;; TODO: `persp-projectile' doesn't seem to work: (1) doesn't change
;; perspective when switching projects.  (2) when in perspective
;; finding files from helm-projectile doesn't add them to perspective
;; so helm-mini doesn't show them. BBatsov isn't using perspective so
;; chances of a fix are low. Should I consider using Bookmarks+
;; instead?
(require 'persp-projectile)
;; (define-key projectile-mode-map (kbd "...")  'projectile-persp-switch-project)


;; TODO: make sense of Virtual Directories
;; see `http://tuhdo.github.io/helm-projectile.html'

;; TODO: set these `grep-find-ignored-files' `grep-find-ignored-directories'
;; `projectile-globally-ignored-files' `projectile-globally-ignored-directories'
;; NOTE the need to do this to force `helm-projectile-grep' behave properly is
;; disconcerting see `https://github.com/bbatsov/projectile/issues/628'
(add-to-list 'grep-find-ignored-directories "elpa")
(add-to-list 'grep-find-ignored-directories "node_modules")

;; TODO "C-c p l" `projectile-find-file-in-directory' is broken (could be that
;; helm-interface is messed up)

;; TODO use that to propertize the `projectile-mode-line', see `appearance.el'
(defface projectile-selected-face
  '((t (:inherit default)))
  "The face used to highlight the current projectile on the modeline.")

(provide 'setup-projectile)
