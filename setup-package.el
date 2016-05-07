(require 'package)

;; Add melpa to package repos
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (file-exists-p (concat user-emacs-directory "elpa/archives/melpa"))
  (package-refresh-contents))

;; install required packages
(let ((install #'(lambda (package)
                   (unless (package-installed-p package)
                     (package-install package))
                   (require package))))
  (message "Installing required packages %s" ze/required-packages)
  (mapc install ze/required-packages)
  (delete-other-windows))

(defun packages-install (packages)
  (require 'dash)
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))


(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(provide 'setup-package)
