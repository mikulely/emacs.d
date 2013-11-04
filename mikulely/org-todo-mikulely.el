;;; org-todo-mikulely --- org-mode settings for GTD
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Created: 2013/06/14 04:01:08
;; Keywords: todo

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
;;  Using Emacs For GTD

;;; Code:

;;  关于设置: (a@/!)
;; a 为快捷键
;; @: 切换时需要文字说明
;; !: 切换时自动插入时间
;; | : 分割未完成和已经完成任务
;; 任务状态
(setq org-todo-keywords
      (quote (
              ;; 私人还是公司？
              (sequence "TODO(t)" "PROJECT(P)")
              ;; 进行的状态
              (sequence "INPROGRESS(s!)" "|" "IMMEDIATELY(i)" "DONE(d@/!)")
              ;; 受阻的状态
              (sequence "WAITING(w@/!)" "|" "CANCELED(c@/!)" "TODO(t@)" )
              ;; 委托给他人的状态
			  (sequence "DEFERRED(y@/!)" "|" "CANCELED(c@/!)" "GOT(g@/!)")
              ;; 处理Bug
              ;; (sequence "REPORT(r!)" "KNOWNCAUSE(k!)" "|" "FIXED(f!)")
              ;; 进一步处理
			  (sequence "Testing" "|" "Coding" "Documentation")
			  (sequence "OPEN@BUG" "INVESTIGATE@BUG" "FIX@BUG"
                        "TEST@BUG" "REVIEW@BUG" "CLOSE@BUG" "|"
                        "FIXED@BUG" "TRANSFERRED@BUG")
              )))

;; I have a few triggers that automatically assign tags to tasks
;; based on state changes.
(setq org-todo-state-tags-triggers
      '(
        ;; Moving a task to TODO removes CANCELLED and WAITING tags
        ("TODO" ("WAITING") ("CANCELLED"))
        ;;Moving a task to CANCELLED adds a CANCELLED tag
        ("CANCELLED" ("CANCELLED" . t))
        ;; Moving a task to DONE removes WAITING and CANCELLED tags
        ("DONE" ("WAITING") ("CANCELLED") ("DEFERRED") )
))

(setq org-todo-keyword-faces
      '(
        ("TODO" . ((:foreground "red" :weight bold)))
        ("PROJECT" . ((:foreground "blue" :weight bold)))

        ("INPROGRESS" . ((:foreground "gold")))
        ("IMPEDED" . (:foreground "OrangeRed" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMMEDIATELY" . (:foreground "green" :weight bold))

        ;; ("REPORT" . ((:foreground "VioletRed4" :weight bold)))
        ;; ("KNOWNCAUSE" . ((:foreground "DarkGreen")))
        ("FIXED" . ((:foreground "SpringGreen4" :weight bold)))
        ("DEFERRED" . shadow)
        ("WAITING" . (:foreground "gray" :weight bold))
        ("CANCELED" . ((:foreground "ForestGreen" :weight bold)))

        ))

;; With speed commands enabled, I can enter single-letter commands when
;; the cursor is at the very beginning of a headline. ? displays a menu
(setq org-use-speed-commands t)

;; 子节点若有未完成事项，则父节点不能标记为 Done
(setq org-enforce-todo-dependencies t)
;;------------------------------------------------------------------------------
;; 跟踪子任务完成情况
;;------------------------------------------------------------------------------
;; @see http://www.cnblogs.com/holbrook/archive/2012/04/14/2447754.html
;; 即使所有的子任务都完成，也只是标记上一级任务的完成情况为100%，而不能自
;; 动更新上级任务的完成状态。如果需要自动设定为完成，可以在.emacs中增加如
;; 下配置：

;; Org(Top) -> TODO Items -> Breaking down tasks
;; 当子任务完成时，自动地将任务设置为完成
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)



;;; provide features
(provide 'org-todo-mikulely)

;;; org-todo-mikulely.el ends here
