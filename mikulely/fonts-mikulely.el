;;; fonts-mikulely --- font settings for emacs
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Keywords: font

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
;;  Emacs Font Settings

;;; Code:

(require 'cl)


(defun sanityinc/font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun sanityinc/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
Emacs will keep the pixel size of the frame approximately the
same.  DELTA should be a multiple of 10, to match the units used
by the :height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (sanityinc/font-name-replace-size
                         (face-font 'default)
                         new-point-height) t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun sanityinc/increase-default-font-height ()
  (interactive)
  (sanityinc/increment-default-font-height 10))

(defun sanityinc/decrease-default-font-height ()
  (interactive)
  (sanityinc/increment-default-font-height -10))

(global-set-key (kbd "C-M-=") 'sanityinc/increase-default-font-height)
(global-set-key (kbd "C-M--") 'sanityinc/decrease-default-font-height)

;;------------------------------------------------------------------------------
;; font settings
;;------------------------------------------------------------------------------
;; @see http://zhuoqiang.me/torture-emacs.html
;; 判断某个字体在系统中是否存在
(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil
    t))

;; 要到用户预定义的字体列表中找出首个存在的字体：
(defvar font-list '("Microsoft Yahei" "WenQuanYi Micro Hei" "黑体" "新宋体" "宋体"))

(require 'cl) ;; find-if is in common list package
(find-if #'qiang-font-existsp font-list)

;; 用来产生带上字体大小信息的字体描述文本的辅助函数：
(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)

  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl) ; for find if
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                            :size chinese-font-size)))

    ;; Set the default English font
    ;;
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute 'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font))))

(qiang-set-font
  '("Consolas" "Monospace" "DejaVu Sans Mono" "Courier New") ":pixelsize=17"
   '("WenQuanYi Micro Hei" "Microsoft Yahei" "黑体" "新宋体" "宋体"))

;; 字体设置解决了ibuffer在GUI下列无法对齐的问题。

;; 解决Emacs client不使用设置的字体的问题
;; @see http://stackoverflow.com/questions/3984730/emacs-gui-with-emacs-daemon-not-loading-fonts-correctly
;; (setq default-frame-alist '((font . "Monospace-11")))
;; (set-frame-font "Monospace-11")
;; (set-fontset-font "fontset-default" 'han '("WenQuanYi Micro Hei" . "unicode-bmp"))

;;; provide features
(provide 'fonts-mikulely)

;;; fonts-mikulely.el ends here
