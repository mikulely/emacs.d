;;; org-clock-mikulely.el --- Org Clock for Emacs Settings

;; Description: Org Clock for Emacs Settings
;; Created: 2013-10-06 00:46
;; Author: mikulely  <mikulely@gmail.com>
;; Keywords: org-clock
;; URL: https://github.com/mikulely/emacs.d

;; Copyright (C) 2013, mikulely, all rights reserved.

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

;; TODO 把原来的org配置注释掉，自己写
;;------------------------------------------------------------------------------
;; Org clock在 todo.org 中，移到一个条目上，按 Ctrl-c Ctrl-x Ctrl-i 即
;; 可对该条目开始计时，Ctrl-c Ctrl-x Ctrl-o 停止当前计时。如果在
;; Agenda 中，移到条目按 I(大写) 即可对该条目开始计时，O(大写) 即可停止
;; 计时
;; ------------------------------------------------------------------------------
;; Save the running clock and all clock history when exiting Emacs,
;; load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to INPROGRESS when clocking in
(setq org-clock-in-switch-to-state "INPROGRESS")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)



;;; provide features
(provide 'org-clock-mikulely)

;;; org-clock-mikulely.el ends here

