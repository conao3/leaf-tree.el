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

(defcustom leaf-tree-regexp (eval-when-compile
                              (require 'regexp-opt)
                              (concat "^\\s-*("
                                      (regexp-opt '("leaf") 'symbols)
                                      "\\s-+\\("
                                      (or (bound-and-true-p lisp-mode-symbol-regexp)
                                          "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
                                      "\\)"))
  "Regexp serch `leaf'.
Regexp must have 2 group, for OP and LEAF--NAME.
See `leaf-enable-imenu-support' to reference regexp."
  :group 'leaf-tree
  :type 'string)


;;; Function

(defvar leaf-tree-mode)
(defvar leaf-tree--imenu--index-alist nil)

(defun leaf-tree--imenu--list-rescan-imenu ()
  "Create `leaf' index alist for the current buffer.

This function modify `leaf-tree--imenu--index-alist'
instead of `imenu--index-alist' as same format.

`imenu--index-alist' is alist like below format.
  imenu--index-alist := TREE
    TREE    := nil | (<GROUP | NODE>*)
    GROUP   := (<GROUP | (G_TITLE NODE+)>*)
    NODE    := (N_TITLE . MARKER)
    G_TITLE := <string>      ; Group title
    N_TITLE := <string>      ; Node title
    MARKER  := <marker>      ; Marker at definition beggining

This function is minor change from `imenu--make-index-alist'."
  (let (ret)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward leaf-tree-regexp nil t)
        (let ((beg (match-beginning 0))
              ;; (op (match-string 1))
              (leaf--name (match-string 2)))
          (push `(,leaf--name . ,(set-marker (make-marker) beg)) ret))))
    (setq leaf-tree--imenu--index-alist `(("leaf-tree" ,@(nreverse ret))))))


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
    (leaf-tree--imenu--list-rescan-imenu) ; renew `leaf-tree--imenu--index-alist'
    (setq imenu-list--imenu-entries leaf-tree--imenu--index-alist)
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
