;;; org-capture-mikulely --- org-capture settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Created: 2013/06/14 04:01:08
;; Keywords: org-capture

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
;;  Using Emacs as CollectionBox

;;; Code:

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;;------------------------------------------------------------------------------
;; EasyPG for capture passwd
;; Setup for transparent, automatic encryption and decryption:
;; @see http://www.emacswiki.org/emacs/EasyPG
;;------------------------------------------------------------------------------
(require 'epa)
;; Using symmetric encryption always
;; To prevent EPG from prompting for a key every time you save a file
(setq epa-file-encrypt-to nil)
(setq epa-file-cache-passphrase-for-symmetric-encryption t) ;; 允许缓存密码，否则编辑时每次保存都要输入密码
(setq epa-file-inhibit-auto-save nil) ;; 允许自动保存

(setq system-time-locale "en_US") ;; 保证时间戳时英文的

(setq org-directory "~/Dropbox/org/")
(setq org-capture-templates '(
                              ;; %i initial content, the region when capture is called while the
                              ;; region is active. The entire text will be indented like `%i' itself.
                              ;; %a annotation, normally the link created with `org-store-link'.

                              ;; Note how the templates are spread across multiple lines. If you
                              ;; want some text to start on a new line then format the template
                              ;; accordingly. For example the "Added:" text starts on a new line.
                              ;; @see http://members.optusnet.com.au/~charles57/GTD/datetree.html

                              ;; GTD
                              ;; When I start a capture mode task the task is clocked in as
                              ;; specified by :clock-in t and when the task is filed with C-c C-c
                              ;; the clock resumes on the original clocking task.

                              ;; 为了使各个分类项目的标题在日程表（Agenda View）中显示，需要在项目名
                              ;; 称下面写上「#+CATEGORY」。
                              ("s" "SOMEDAY" entry (file+headline
                                                    "~/Dropbox/org/gtd/someday.org" "To be Sorted")
                               "* TODO %^{想做什么？请写上清晰的执行动作}  %?":prepend t :empty-lines 1
                               )

                              ;; 需要在指定日期完成的事情
                              ("w" "Work" entry (file+datetree
                                                        "~/Dropbox/org/nsfocus/nsfocus_diary.org")
                               "* Coding %^{在做什么？}  %?":prepend t :empty-lines 1
                               )
                              ;; %^g prompts for a tag
                              ;; %u, %U like the above, but inactive timestamps.
                              ;; %a 加入你插如条目时所在的文件

                              ;; %c Current kill ring head.
                              ;; %x Content of the X clipboard.

                              ;; 记录笔记
                              ("n" "Journal" entry (file+datetree
                                                    "~/Dropbox/org/gtd/journal.org")
                               "* NOTE %^{笔记主题} %? " :prepend t :empty-lines 1
                               )
                              ;; 密码归档
                              ("p" "Passwd" table-line (file+headline "~/Dropbox/org_archive/account.org.gpg" "Passwd")
                               "|%^{Account For}|%^{User Name}|%^{Passwd}|%U|" )

                              ;; %t timestamp, date only.
                              ;; %T timestamp with date and time.

                              ;; 财务管理
                              ;; ("f" "Financial" table-line (file+headline
                              ;;                              "~/Dropbox/org/life/financial.org" "Financial")
                              ;;  "|%u|%^{For What？}|%^{How much?}|" :prepend t :empty-lines 1
                              ;;  )

                              ;; 单词管理
                              ("e" "English" entry (file+datetree
                                                    "~/Dropbox/org/life/english.org")
                               "* %^{What you've learnt? Dictate it} \n%c " :prepend t :empty-lines 1
                               )
                              ;; When I need to decide what to do next, I use the following methods:
                              ;; + Agenda view - identify items marked with Scheduled or Deadline dates
                              ;; + Tag view - show tems marked with a tag (my GTD contexts, eg HOME, OFFICE, FINANCE, READING, DVD)
                              ;; + Reviewing this weeks nodes - Each day I quickly scan the items added since the beginning of the week.

                              ))

(setq org-tag-alist '((:startgroup . nil)
                      ("@Office"     . ?w)
                      ("@Errand"     . ?h)
                      (:endgroup     . nil)
                      ("Urgent"      . ?u)
                      ("email"       . ?e)
                      ("phone"       . ?p)
                      ("coding"      . ?c)
                      ("reading"     . ?r)
                      ("documenting" . ?d)
                      ("manipulate"  . ?x)
                      ))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)


;;; provide features
(provide 'org-capture-mikulely)

;;; org-capture-mikulely ends here
