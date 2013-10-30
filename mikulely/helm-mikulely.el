;;; helm-mikulely.el --- helm config

;; Description: helm config
;; Created: 2013-09-06 20:19
;; Author: Jiaying Ren  <mikulely@gmail.com>
;; Keywords: helm
;; URL: https://github.com/mikulely/emacs.d

;; Copyright (C) 2013, Jiaying Ren, all rights reserved.

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

;;

;;; Code:

(require-package 'helm)
(require-package 'helm-ls-git)
(require-package 'helm-cmd-t)
(require-package 'helm-gtags)
(require-package 'helm-c-yasnippet)
(require-package 'helm-dired-recent-dirs)

;; @see http://emacs-helm.github.io/helm/
(require 'helm-config)

(setq helm-c-adaptive-history-file "~/.emacs.d/cache/helm-c-adaptive-history")


;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(setq helm-c-gtags-path-style 'relative)
(setq helm-c-gtags-ignore-case t)
(setq helm-c-gtags-read-only t)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)
              (local-set-key (kbd "C-c C-f") 'helm-gtags-pop-stack)))
;; ==end

;;; provide features
(provide 'helm-mikulely)

;;; helm-mikulely.el ends here
