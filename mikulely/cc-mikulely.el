;;; cc-mikulely --- c programmling language family settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Keywords: C C++

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
;; Using Emacs for C/C++ Programmling

;;; Code:

;; google-style
(require-package 'google-c-style)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; (defun google-c-indent-hook ()
;;   (setq c-basic-offset 4
;;         tab-width 4))

;; (add-hook 'c-mode-common-hook 'google-c-indent-hook)
;; (add-hook 'c++-mode-common-hook 'google-c-indent-hook)

;; Tell emacs to open .h file in c++ mode, default is c mode
(add-to-list 'auto-mode-alist '("\\.[Hh][Xx][Xx]$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.c$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
;; 默认使用 linux 风格
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "linux")))

(defun cpplint ()
  "check source code format according to Google Style Guide"
  (interactive)
  (compilation-start (concat "python2.7 ~/.emacs.d/scripts/cpplint.py " (buffer-file-name))))

;; 自动给 C 代码加上头文件保护
;; @http://jmdavisblog.blogspot.com/2013/08/a-handful-of-emacs-utilities.html
(defun get-include-guard ()
  "Return a string suitable for use in a C/C++ include guard"
  (let* ((fname (buffer-file-name (current-buffer)))
         (fbasename (replace-regexp-in-string ".*/" "" fname))
         (inc-guard-base (replace-regexp-in-string "[.-]"
                                                   "_"
                                                   fbasename)))
    (concat (upcase inc-guard-base) "_")))

(add-hook 'find-file-not-found-hooks
          '(lambda ()
             (let ((file-name (buffer-file-name (current-buffer))))
               (when (string= ".h" (substring file-name -2))
                 (let ((include-guard (get-include-guard)))
                   (insert "#ifndef " include-guard)
                   (newline)
                   (insert "#define " include-guard)
                   (newline 4)
                   (insert "#endif")
                   (newline)
                   (previous-line 3)
                   (set-buffer-modified-p nil))))))


;;------------------------------------------------------------------------------
;; ggtags
;; @see https://github.com/leoliu/ggtags
;;------------------------------------------------------------------------------
(require-package 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1)
              )))

;;------------------------------------------------------------------------------
;; ctypes-setting
;;------------------------------------------------------------------------------
(require-package 'ctypes)
(ctypes-auto-parse-mode 1)
;; (setq ctypes-write-types-at-exit t)
;; (setq ctypes-file-name "~/.emacs.d/cache/ctypes")
;; (ctypes-read-file nil nil t t)

;;------------------------------------------------------------------------------
;; c-eldoc
;;------------------------------------------------------------------------------
(require-package 'c-eldoc)
(require 'c-eldoc)
;; add in your commonly used packages/include directories here, for
;; example, SDL or OpenGL. this shouldn't slow down cpp, even if
;; you've got a lot of them
;; (add-hook 'c-mode-common-hook
;;           '(lambda ()
;;              (when (require 'c-eldoc nil 'noerror)
;;                (setq c-eldoc-includes "`pkg-config gtk+-3.0 glib-2.0 --cflags --libs` -I./ -I../")
;;                (c-turn-on-eldoc-mode))))

;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;;------------------------------------------------------------------------------
;; global
;;------------------------------------------------------------------------------
;; This function will check if there is an existing tagfile; if so, it
;; will update it. If not, it asks where to create the tag file; you
;; should provide the name of the top of your source directory there.
;; @see http://emacs-fu.blogspot.com/2009/01/navigating-through-source-code-using.html
(defun djcb-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((olddir default-directory)
            (topdir (read-directory-name
                     "gtags: top of source tree:" default-directory)))
        (cd topdir)
        (shell-command "gtags && echo 'created tagfile'")
        (cd olddir)) ; restore
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'")))

;; Now, we can automatically run this function whenever we open a
;; C/C++/...-file, so we always have an up-to-date tagfile available:
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (not (string-match "/usr/src/minix" (expand-file-name default-directory)))
;;               (djcb-gtags-create-or-update))))

;; (require 'hideif)
;; (setq hide-ifdef-initially t)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (setq hide-ifdef-shadow t)
;;             (setq hide-ifdef-shadow nil)
;;             (setq hide-ifdef-mode t)
;;             (hide-ifdefs)
;;             ))

;; (require 'init-hideif-table)

;; When I'm writing some code, I find it often useful to hide most
;; function bodies, and only see the one I am working on.
(add-hook 'c-mode-common-hook
          (lambda ()
            (hs-minor-mode t)
            (hs-hide-all)
            ))

;;gdb-many-window
;; (setq gdb-many-windows t)
;; (global-set-key [f6] 'gud-step)
;; (global-set-key [f7] 'gud-next)
;; (global-set-key [f8] 'gud-finish)
;;http://www.inet.net.nz/~nickrob/multi-gud.el
;;http://www.inet.net.nz/~nickrob/multi-gdb-ui.el

;; TODO cscope
;; ==================== cscope ====================
;; +----------+--------------------------------------------------+
;; |C-c s a |设定初始化的目录，一般是你代码的根目录 |
;; +----------+--------------------------------------------------+
;; |C-s s I |对目录中的相关文件建立列表并进行索引 |
;; +----------+--------------------------------------------------+
;; |C-c s s |序找符号 |
;; +----------+--------------------------------------------------+
;; |C-c s g |寻找全局的定义 |
;; +----------+--------------------------------------------------+
;; |C-c s c |看看指定函数被哪些函数所调用 |
;; +----------+--------------------------------------------------+
;; |C-c s C |看看指定函数调用了哪些函数 |
;; +----------+--------------------------------------------------+
;; |C-c s e |寻找正则表达式 |
;; +----------+--------------------------------------------------+
;; |C-c s f |寻找文件 |
;; +----------+--------------------------------------------------+
;; |C-c s i |看看指定的文件被哪些文件include |
;; (require 'xcscope)
;; (require 'xcscope+)
;; (setq cscope-do-not-update-database t); +----------+--------------------------------------------------+

;; TODO auto-complete-clang 配置
;; (require 'auto-complete-clang)

;;   (add-hook 'c++-mode-hook 'ac-cpp-mode-setup)
;;   (defun ac-cpp-mode-setup ()
;;     "auto-complete settings for c-mode."
;;     (setq ac-sources '(
;; ;;                       ac-source-clang
;;                        ac-source-dictionary
;;                        ac-source-abbrev
;;                        ac-source-semantic
;;                        ac-source-filename
;;                        ac-source-files-in-current-dir
;;                        ac-source-words-in-same-mode-buffers
;;                        )))
;; Default clang completion flags
;; (setq ac-clang-flags
;;       (split-string
;;        (concat
;;         "-pthread -I./ -I../ "
;;         (shell-command-to-string "pkg-config --cflags-only-I opencv gtk+-3.0"))))

;;; provide features
(provide 'cc-mikulely)

;;; cc-mikulely.el ends here
