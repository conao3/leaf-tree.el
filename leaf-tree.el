;;; leaf-tree.el --- Interactive folding Elisp code using :tag leaf keyword  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience leaf
;; Package-Requires: ((emacs "25.1") (imenu-list "0.8"))
;; URL: https://github.com/conao3/leaf-tree.el

;; This program is free software: you can redistribute it and/or modify
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

;; Interactive folding Elisp code using :tag leaf keyword.


;;; Code:

(require 'imenu-list)

(defgroup leaf-tree nil
  "Interactive folding Elisp code using :tag leaf keyword."
  :prefix "leaf-tree-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/leaf-tree.el"))


;;; Advice

(defvar leaf-tree-advice-alist
  '((imenu-list-update . leaf-tree--advice-imenu-list-update))
  "Alist for leaf-tree advice.
See `leaf-tree--setup' and `leaf-tree--teardown'.")

(defun leaf-tree--advice-imenu-list-update (fn &rest args)
  "Around advice for FN with ARGS.
This code based `imenu-list' (2019/03/15 hash:4600873)
See `imenu-list-update'."
  (apply fn args))


;;; Main

(defun leaf-tree--setup ()
  "Setup leaf-tree."
  (pcase-dolist (`(,sym . ,fn) leaf-tree-advice-alist)
    (advice-add sym :around fn)))

(defun leaf-tree--teardown ()
  "Teardown leaf-tree."
  (pcase-dolist (`(,sym . ,fn) leaf-tree-advice-alist)
    (advice-remove sym fn)))

(define-minor-mode leaf-tree-mode
  "Toggle `leaf' specific customize for `imenu-list'."
  :require 'leaf-tree
  :group 'leaf-tree
  :lighter " leaf-tree"
  (if leaf-tree-mode
      (leaf-tree--setup)
    (leaf-tree--teardown)))

(provide 'leaf-tree)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-tree.el ends here
