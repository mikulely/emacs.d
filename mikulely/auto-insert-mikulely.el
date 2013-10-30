;;; auto-insert-mikulely --- auto insert settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Created: 2013/06/17 02:33:01
;; Keywords: auto insert, template

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; see the file COPYING. If not,
;; write to the Free Software Foundation, Inc., 51 Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;  Auto Insert Mode as A Template System

;;; Code:

(require-package 'yasnippet)
(require-package 'dropdown-list)

(setq yas/snippet-dirs "~/.emacs.d/snippets")

;; give yas/dropdown-prompt in yas/prompt-functions a chance
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                              yas-ido-prompt
                              yas-completing-prompt))
(yas-global-mode 1)

;; 设置auto-insert 的模版存放目录
(setq auto-insert-directory (concat (getenv "HOME") "/.emacs.d/auto-insert/"))

(add-hook 'find-file-hooks 'auto-insert)
(auto-insert-mode)  ;; 启用auto-insert

;; 默认插入模版前会循问你要不要自动插入，这里设置为不必询问，
(setq auto-insert-query nil)

(setq user-full-name "Jiaying Ren")
(setq user-mail-address "mikulely@gmail.com")

;; (define-auto-insert "\\.org" "org-auto-insert")
(define-auto-insert "\\.el$" "el-auto-insert")
(define-auto-insert "\\.h$" "h-auto-insert")
(define-auto-insert "\\.c$" "c-auto-insert")
(define-auto-insert "\\.cpp$" "c++-auto-insert")
(define-auto-insert "\\.cc$" "c++-auto-insert")
(define-auto-insert "\\.py$" "py-auto-insert")
(define-auto-insert "MAKEFILE" "mikefile-auto-insert")

;; @see http://jixiuf.github.io/emacs/auto-insert-and-yasnippet.html
(defadvice auto-insert  (around yasnippet-expand-after-auto-insert activate)
  "expand auto-inserted content as yasnippet templete, so that we could use yasnippet in autoinsert mode"
  (let ((is-new-file (and (not buffer-read-only)
                          (or (eq this-command 'auto-insert)
                              (and auto-insert   (< (buffer-size) 100)
                                   (not (string-match "[^ \t\n\r]+" (buffer-string))))))))

    ad-do-it
    (let ((old-point-max (point-max)))
      (when is-new-file
        (goto-char old-point-max)
        (yas-expand-snippet (buffer-substring-no-properties (point-min) (point-max)))
        (delete-region (point-min) old-point-max)
        )
      )
    )
  )

;;; provide features
(provide 'auto-insert-mikulely)


;;; auto-insert-mikulely.el ends here
