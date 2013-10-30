;;; org-agenda-mikulely.el --- Org-mode Agenda Settings

;; Description: Org-mode Agenda Settings
;; Created: 2013-09-30 14:02
;; Author: Jiaying Ren  <mikulely@gmail.com>
;; Keywords: ageda
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


(setq org-agenda-custom-commands
      '(
        ("P" "Projects"
         ((tags-todo "PROJECT")))
        ("W" "Waiting"
         ((tags-todo "WAITING")))
        ("D" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up))))
                      (org-deadline-warning-days 0)))))
        ;;
        ("l" "Agenda and all TODO's"
         ((alltodo)))

        ;; notes to archived
        ("n" "Notes to be Archived" todo "NOTE")

        ;; entries to archived
        ("A" "Tasks to be Archived" todo "DONE|CANCELLED|GOT")

        ))

;;; provide features
(provide 'org-agenda-mikulely)

;;; org-agenda-mikulely.el ends here

