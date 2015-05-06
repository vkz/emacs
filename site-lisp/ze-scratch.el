;;; ze-scratch.el --- My personal scratch buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

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

;; My personal scratch buffer, with a nice logo.

;;; Code:

(defconst ze/logo-file (locate-user-emacs-file "logo.png")
  "The path to my logo.")

(defconst ze/logo-url "https://avatars3.githubusercontent.com/u/917220?v=3&s=140"
  "The URL of my logo.")

;;;###autoload
(defun ze/insert-logo ()
  "Insert my logo into the current buffer."
  (interactive)
  (unless (file-exists-p ze/logo-file)
    (url-copy-file ze/logo-url ze/logo-file
                   nil 'keep-time))
  (insert-image (create-image ze/logo-file) "logo")
  (insert "\n"))

;;;###autoload
(defun ze/insert-logo-into-scratch ()
  "Insert my logo into the scratch buffer."
  (with-current-buffer "*scratch*"
    (goto-char (point-max))
    (insert "\n")
    (ze/insert-logo)))

(provide 'ze-scratch)
;;; ze-scratch.el ends here
