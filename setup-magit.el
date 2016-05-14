(require 'magit)

(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-unstage-all-confirm nil)

(use-package gist
  :ensure t
  :bind (;; ("C-c l" . gist-list)
         ("C-c g" . gist-region-or-buffer-private)
         ("C-c G" . gist-region-or-buffer)))

(provide 'setup-magit)
