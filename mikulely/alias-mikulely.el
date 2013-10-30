;;; alias-mikulely --- define personal alias
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Keywords: alias

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
;; Using Emacs for Alias

;;; Code:

;; git-messenger.el provides function that popup commit message at
;; current line. This is useful when you want to know why this line
;; was changed.
(defalias 'why    'git-messenger:popup-message)

; elisp
(defalias  'eb  'eval-buffer)
(defalias  'er  'eval-region)
(defalias  'ed  'eval-defun)
(defalias  'eis 'elisp-index-search)
(defalias  'lf  'load-file)

; mode
(defalias  'hm    'html-mode)
(defalias  'tm    'text-mode)
(defalias  'elm   'emacs-lisp-mode)
(defalias  'om    'org-mode)
(defalias  'ssm   'shell-script-mode)
(defalias  'wd    'wdired-change-to-wdired-mode)
(defalias  'wg    'wgrep-change-to-wgrep-mode)

(defalias  'go    'helm-google-suggest)
(defalias  'yt    'youtube)

;; external cmd
(defalias  'p     'ping)
(defalias  'd     'dig)
(defalias  'i     'ifconfig)

(defalias  'insert-line-number   'rectangle-number-lines)
(defalias  'w                    'save-some-buffers)
(defalias  'pi                   'package-install)
(defalias  'pl                   'package-list-packages)
(defalias  'db                   'douban-music)
(defalias  'C                    'clean-buffer-list)
(defalias  'st                   'scratch-toggle)
(defalias  'h2v                  'window-vertical-to-horizontal)
(defalias  'v2h                  'window-horizontal-to-vertical)
(defalias  'lisp                 'ielm)
(defalias  'blog                 'octopress)
(defalias  'snake                'snakecase-word-or-region)
(defalias  'camel                'camelcase-word-or-region)
(defalias  'cw                   'count-words)

(provide 'alias-mikulely)

;;; alias-mikulely.el ends here
