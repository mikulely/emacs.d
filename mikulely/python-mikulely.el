;;; python-mikulely.el --- python mode settings

;; Description: python mode settings
;; Created: 2013-09-06 08:54
;; Author: Jiaying Ren  mikulely@gmail.com
;; Keywords: python
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

(add-hook 'python-mode-hook
          '(lambda ()
             (set-variable 'python-indent-offset 4)
             (set-variable 'python-indent-guess-indent-offset nil)
             )
          )

;;; provide features
(provide 'python-mikulely)

;;; python-mikulely.el ends here
