;;; editing-mikulely --- editing settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Keywords: fic-mode, tab, fill-column

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
;; Emacs Editing Settings

;;; Code:

;; Some Basic Preferences
(setq-default
 mouse-yank-at-point t              ;; 粘贴于光标处, 而不是鼠标指针处
 x-select-enable-clipboard t        ;; 允许 emacs 和外部其他程序的粘贴
 auto-compression-mode 1            ;; 自动解压缩文件并访问其内容
 auto-image-file-mode t             ;; 打开图片显示功能
 undo-outer-limit 50000000          ;; 撤销限制
 message-log-max t                  ;; 设置 message 记录全部消息, 而不用截去
 initial-scratch-message ""
 ;; Make scrolling not suck.
 scroll-margin 0
 scroll-conservatively 100000
 scroll-up-aggressively 0
 scroll-down-aggressively 0
 visible-bell t)

(setq search-highlight           t) ; Highlight search object
(setq query-replace-highlight    t) ; Highlight query object

(setq default-major-mode 'text-mode)

                                        ; @see https://github.com/jhamrick/emacs/blob/master/.emacs.d/settings/general-settings.el
                                        ; turn on mouse wheel support for scrolling
(require 'mwheel)
(mouse-wheel-mode 1)

;;----------------------------------------------------------------------------
;; 24 版本中新的 minor mode，相当于原来的 auto-pair。
;; setting for auto-close brackets for electric-pair-mode regardless
;; of current major mode syntax table
;; ----------------------------------------------------------------------------
(when (fboundp 'electric-pair-mode)
  (setq-default electric-pair-mode 1))

(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            (?\` . ?\`)
                            ))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (set (make-local-variable 'electric-pair-mode) nil)))


;; but don't show trailing whitespace in servral modes.
(dolist (hook '(term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                emms-playlist-mode-hook
                mu4e-view-mode-hook
                mu4e-headers-mode-hook
                mu4e-compose-mode-hook
                calendar-mode-hook
                twittering-mode-hook
                erc-mode-hook
                fundamental-mode-hook
                package-menu-mode-hook
                org-octopress-summary-mode-hook
                Info-mode-hook
                douban-music-mode-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

(transient-mark-mode t)

;; fix backward-up-list to understand quotes, see http://bit.ly/h7mdil
(defun backward-up-sexp (arg)
  "jump up to the start of the arg'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; c-m-u, c-m-up

(require-package 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; 缩进设置
(setq tab-stop-list ())
;; Don't insert tabs
(setq-default indent-tabs-mode nil)
;; 不用 TAB 字符来 indent, 这会引起很多奇怪的错误。编辑 Makefile 的时候
;; 也不用担心，因为 makefile-mode 会把 TAB 键设置成真正的 TAB 字符，并
;; 且加亮显示的。
(setq-default tab-width 4)        ; 设置 TAB 默认的宽度
(dolist (hook (list               ; 设置用空格替代 TAB 的模式
               'emacs-lisp-mode-hook
               'lisp-mode-hook
               'org-mode-hook
               'lisp-interaction-mode-hook
               'scheme-mode-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'asm-mode-hook
               'emms-tag-editor-mode-hook
               'sh-mode-hook))
  (add-hook hook '(lambda () (setq indent-tabs-mode nil))))

;; 自动换行
(setq fill-column 90)          ; 默认显示 90 列就换行
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               'org-mode-hook
               'mu4e-compose-mode-hook
               'mu4e-view-mode-hook
               ))
  (add-hook hook '(lambda () (auto-fill-mode 1))))

;;设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;----------------------------------------------------------------------------
;; Add space between Chinese and English characters.
;; @https://github.com/coldnew/pangu-spacing
;;----------------------------------------------------------------------------
(require-package 'pangu-spacing)
(global-pangu-spacing-mode 1)
;; 只有在 org-mode 里真的添加空白
(add-hook 'org-mode-hook
          '(lambda ()
             (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

;; fic-mode
(require-package 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

;; Emacs中用 firefox浏览器打开超链接
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; @see http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/?utm_source=Python+Weekly+Newsletter&utm_campaign=611942dc15-Python_Weekly_Issue_107_October_3_2013&utm_medium=email&utm_term=0_9e26887fc5-611942dc15-312672681%29
(require-package 'fill-column-indicator)
(setq-default fci-rule-column 80)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode t)
(add-hook 'prog-mode-hook
          (lambda ()
            (fci-mode t))
          't)


;;; provide features
(provide 'editing-mikulely)

;;; editing-mikulely.el ends here
