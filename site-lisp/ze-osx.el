;;; ze-osx.el --- Utilities for OS X          -*- lexical-binding: t; -*-

;; Copyright (c) 2012-2015 Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://gihub.com/lunaryorn/.emacs.d

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide additional utilities for OS X

;;; Code:

;; Trash support for OS X.  On OS X, Emacs doesn't support the system trash
;; properly, so we try to work around it by providing our own trashing function.
;; If that fails, disable trashing and warn!
(defconst ze/trash-tool "trash"
  "A CLI tool to trash files.")

(defun ze/move-file-to-trash (file)
  "Move FILE to trash on OS X."
  (call-process ze/trash-tool nil nil nil (expand-file-name file)))

(defun ze/homebrew-prefix (&optional formula)
  "Get the homebrew prefix for FORMULA.

Without FORMULA, get the homebrew prefix itself.

Return nil, if homebrew is not available, or if the prefix
directory does not exist."
  (let ((prefix (ignore-errors (car (apply #'process-lines "brew" "--prefix"
                                           (when formula (list formula)))))))
    (when (and prefix (file-directory-p prefix))
      prefix)))

(defun ze/homebrew-installed-p (&optional formula)
  "Determine whether a homebrew FORMULA is installed.

Without FORMULA determine whether Homebrew itself is available."
  (if formula
      (ze/homebrew-prefix formula)
    (executable-find "brew")))

(provide 'ze-osx)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; ze-osx.el ends here
