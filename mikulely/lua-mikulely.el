;;; lua-mikulely.el --- Emacs Settings for lua

;; Description: Emacs Settings for lua
;; Created: 2013-10-05 23:13
;; Author: Jiaying Ren  <mikulely@gmail.com>
;; Keywords: lua
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

(require-package 'lua-mode)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require-package 'flymake-lua)
(add-hook 'lua-mode-hook 'flymake-lua-load)

(setq lua-indent-level 4)

;;; provide features
(provide 'lua-mikulely)

;;; lua-mikulely.el ends here
