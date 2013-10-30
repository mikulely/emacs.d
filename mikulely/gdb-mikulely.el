;;; gdb-mikulely.el --- GDB for Emacs Settings

;; Description: GDB for Emacs Settings
;; Created: 2013-10-06 00:07
;; Author: Jiaying Ren  <mikulely@gmail.com>
;; Keywords: gdb
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

(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gud-chdir-before-run nil)
(setq gud-tooltip-mode t)

;;; provide features
(provide 'gdb-mikulely)

;;; gdb-mikulely.el ends here

