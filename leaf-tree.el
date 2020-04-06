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

(require 'seq)
(require 'imenu-list)

(defgroup leaf-tree nil
  "Interactive folding Elisp code using :tag leaf keyword."
  :prefix "leaf-tree-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/leaf-tree.el"))


;;; Function

(defvar leaf-tree-mode)


;;; Advice

(defvar leaf-tree-advice-alist
  '((imenu-list-collect-entries . leaf-tree--advice-imenu-list-collect-entries))
  "Alist for leaf-tree advice.
See `leaf-tree--setup' and `leaf-tree--teardown'.")

(defun leaf-tree--advice-imenu-list-collect-entries (fn &rest args)
  "Around advice for FN with ARGS.
This code based `imenu-list' (2019/03/15 hash:4600873)
See `imenu-list-collect-entries'."
  (if (not leaf-tree-mode)
      (apply fn args)
    (imenu-list-rescan-imenu)
    (setq imenu-list--imenu-entries imenu--index-alist)
    (setq imenu-list--displayed-buffer (current-buffer))))


;;; Main

(defvar leaf-tree--imenu-list-minor-mode-value nil
  "Stored `imenu-list-minor-mode' value before minor-mode enable.")

(defun leaf-tree--setup ()
  "Setup leaf-tree."
  (setq leaf-tree--imenu-list-minor-mode-value (if imenu-list-minor-mode 1 -1))
  (imenu-list-minor-mode)
  (pcase-dolist (`(,sym . ,fn) leaf-tree-advice-alist)
    (advice-add sym :around fn)))

(defun leaf-tree--teardown ()
  "Teardown leaf-tree."
  (imenu-list-minor-mode leaf-tree--imenu-list-minor-mode-value)
  (pcase-dolist (`(,sym . ,fn) leaf-tree-advice-alist)
    (advice-remove sym fn)))

;;;###autoload
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
