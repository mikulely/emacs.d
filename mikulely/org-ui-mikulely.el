;;; org-ui-mikulely --- UI settings for org-mode
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Created: 2013/06/14 04:01:08
;; Keywords: org-mode, faces

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
;;  Emacs UI Settings

;;; Code:

;; fontify code in code blocks
(global-font-lock-mode 1)
(setq org-src-fontify-natively t)


(require-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; (add-hook 'org-mode-hook
;;             (lambda ()
;;                   (hl-line-mode t))
;;               't)

;; @see http://sachachua.com/blog/2012/12/emacs-strike-through-headlines-for-done-tasks-in-org/
;; Strike through headlines for DONE tasks in Org
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                             :weight normal
                             :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon" :strike-through t)))))



;; @http://whattheemacsd.com/
;;  So I get a little annoyed when the [17/23] cookies at the parent level aren't updated when I remove an item.
;; This code fixes that.
(defun myorg-update-parent-cookie ()
    (when (equal major-mode 'org-mode)
          (save-excursion
                  (ignore-errors
                            (org-back-to-heading)
                                    (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
    (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
    (myorg-update-parent-cookie))


;;; provide features
(provide 'org-ui-mikulely)

;;; org-ui-mikulely.el ends here
