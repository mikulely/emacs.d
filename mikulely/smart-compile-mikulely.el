;;; smart-compile-mikulely --- smart compile settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Created: 2013/06/14 04:01:08
;; Keywords: smart-compile

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
;;  Smart Compile Source Code for Emacs

;;; Code:

(require-package 'smart-compile)

(require 'smart-compile)
;; @see https://github.com/jetspeed/emacs/blob/master/smart-compile-init.el
;; smart-executable-alist 是用来在调用 smart-run 时是否需要 compile。所以
;; 脚本一般都要加入到这个列表中。除非你只用 smart-compile 运行。
;; %F absolute pathname ( /usr/local/bin/netscape.bin )
;; %f file name without directory ( netscape.bin )
;; %n file name without extention ( netscape )
;; %e extention of file name ( bin )
(when (featurep 'smart-compile)
  (setq smart-compile-alist
        '(("\\.c$" . "gcc -g -o %n %f")
          ("\\.[Cc]+[Pp]*$" . "clang++ -g -o %n %f")
          ;; ("\\.[Cc]+[Pp]*$" . "clang++ -std=c++11 -stdlib=libc++ -o %n %f")
          ("\\.java$" . "javac %f")
          ("\\.s$" . "as %f -o %n")
          ("\\.f90$" . "f90 %f -o %n")
          ("\\.[Ff]$" . "f77 %f -o %n")
          ("\\.mp$" . "runmpost.pl %f -o ps")
          ("\\.php$" . "php %f")
          ("\\.tex$" . "latex %f")
          ("\\.l$" . "lex -o %n.yy.c %f")
          ("\\.y$" . "yacc -o %n.tab.c %f")
          ("\\.py$" . "python %f")
          ("\\.sql$" . "mysql < %f")
          ("\\.sh$" . "./%f")
          ("\\.css\\'"   .   "csslint --format=compiler %f")
          (emacs-lisp-mode . (emacs-lisp-byte-compile))))
  )

;;; provide features
(provide 'smart-compile-mikulely)

;;; smart-compile-mikulely.el ends here
