;;; term-mikulely.el --- Emacs Term mode Settings

;; Description: Emacs Term mode Settings
;; Created: 2013-09-30 11:25
;; Author: Jiaying Ren  <mikulely@gmail.com>
;; Keywords: terminal
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

;; @see https://github.com/capitaomorte/yasnippet/issues/289
;; For tab-complete
(add-hook 'term-mode-hook (lambda()
                            (yas-minor-mode -1)
                            (local-unset-key (kbd "<tab>"))
                            ))

;;; provide features
(provide 'term-mikulely)

;;; term-mikulely.el ends here

