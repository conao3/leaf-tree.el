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
  '((imenu-list-update . leaf-tree--advice-imenu-list-update))
  "Alist for leaf-tree advice.
See `leaf-tree--setup' and `leaf-tree--teardown'.")

(defun leaf-tree--advice-imenu-list-update (fn &rest args)
  "Around advice for FN with ARGS.
This code based `imenu-list' (2019/03/15 hash:4600873)
See `imenu-list-update'."
  (if (not leaf-tree-mode)
      (apply fn args)
    (seq-let (raise-imenu-errors force-update) args
      (catch 'index-failure
        (let ((old-entries imenu-list--imenu-entries)
              (location (point-marker)))
          ;; don't update if `point' didn't move - fixes issue #11
          (unless (and (null force-update)
                       imenu-list--last-location
                       (marker-buffer imenu-list--last-location)
                       (= location imenu-list--last-location))
            (setq imenu-list--last-location location)
            (if raise-imenu-errors
                (imenu-list-collect-entries)
              (condition-case err
                  (imenu-list-collect-entries)
                (error
                 (message "imenu-list: couldn't create index because of error: %S" err)
                 (throw 'index-failure err))))
            (when (or force-update
                      ;; check if Ilist buffer is alive, in case it was killed
                      ;; since last update
                      (null (get-buffer imenu-list-buffer-name))
                      (not (equal old-entries imenu-list--imenu-entries)))
              (with-current-buffer (imenu-list-get-buffer-create)
                (imenu-list-insert-entries)))
            (imenu-list--show-current-entry)
            (when imenu-list-auto-resize
              (imenu-list-resize-window))
            (run-hooks 'imenu-list-update-hook)
            nil))))))


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
