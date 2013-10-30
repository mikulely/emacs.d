;;; org-publish-mikulely --- org-publish settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Created: 2013/06/14 04:01:08
;; Keywords: org-publish

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
;;  Using Emacs as Documentation Generator

;;; Code:
;;;
;; (require 'ctable)
;; (require 'orglue)
(require-package 'org-octopress)

;; a_{b} 会被转义
(setq org-export-with-sub-superscripts '{})

;; I like the following hook for turning on things like spell checking
;; @see http://www.windley.com/archives/2010/12/capture_mode_and_emacs.shtml
(add-hook 'org-mode-hook
          (lambda ()
            'turn-on-font-lock
            (setq word-wrap 1)
            ))

(setq org-publish-project-alist
      '(
        ("notes"
         :base-directory "~/Dropbox/org/note/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/Apps/Pancake.io"
         :recursive t
         :publishing-function org-html-publish-to-html
         ;; :html-link-home "index.html"
         ;; :html-link-up "blog-archive.html"
         :headline-levels 5
         :author nil
         :creator-info nil
         :auto-preamble t
         :section-numbers nil                   ;禁止在段落标题前使用数字
         :export-creator-info nil               ;禁止在 postamble 显示"Created by Org"
         :export-author-info nil                ;禁止在 postamble 显示 "Author: Your Name"
         :style-include-default nil             ;禁用默认 css 样式,使用自定义css
         :auto-sitemap nil                      ; Generate sitemap.org automagically...
         :sitemap-filename "random-journal.org" ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Random Journal"        ; ... with title 'Sitemap'.
         :sitemap-sort-files anti-chronologically
         :sitemap-sort-folders last
         :html-head  "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://orgmode.org/worg/style/worg.css\"/>"
         :html-postamble "<p><div style=\"text-align:center\"> © 2013 - Jiaying Ren powered by org-mode and worg. </div></p> "
         )
        ("work-report"
         :base-directory "~/Dropbox/org/nsfocus/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/org/report-nsfocus/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 5
         :export-creator-info nil ;禁止在 postamble 显示"Created by Org"
         :export-author-info nil  ;禁止在 postamble 显示 "Author: Your Name"
         :auto-preamble t
         :section-numbers nil
         :org-html-head-include-default-style nil
         :auto-sitemap nil
         :html-head  "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://orgmode.org/worg/style/worg.css\"/>"
         :html-postamble "<p><div class=\"footer\" style=\"text-align:center\"> © 2013 - Jiaying Ren powered by Emacs and org-mode</div></p> "
         )
        ))

;;------------------------------------------------------------------------------
;; Org-babel
;; @see http://doc.norang.ca/org-mode.html#OrgBabel
;;------------------------------------------------------------------------------
;; (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")
;; (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")

;; (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

;; ; Make babel results blocks lowercase
;; (setq org-babel-results-keyword "results")

;; (defun bh/display-inline-images ()
;;     (condition-case nil
;;               (org-display-inline-images)
;;           (error nil)))

;; (org-babel-do-load-languages
;;  (quote org-babel-load-languages)
;;  (quote (
;;          (emacs-lisp . t)
;;          (dot . t)
;;          (ditaa . t)
;;          (plantuml . t)
;;          (R . t)
;;          (org . t)
;;          (latex . t))))

;;------------------------------------------------------------------------------
;; org-octopress
;;------------------------------------------------------------------------------
(require 'org-octopress)
(setq org-octopress-directory-top       "~/Dropbox/octopress/source")
(setq org-octopress-directory-posts     "~/Dropbox/octopress/source/_posts")
(setq org-octopress-directory-org-top   "~/Dropbox/octopress/source")
(setq org-octopress-directory-org-posts "~/Dropbox/octopress/source/blog")
(setq org-octopress-setup-file          "~/.emacs.d/auto-insert/setup.org")

;;; provide features
(provide 'org-publish-mikulely)

;;; org-publish-mikulely.el ends here
