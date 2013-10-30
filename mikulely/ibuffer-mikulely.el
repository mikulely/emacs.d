;;; ibuffer-mikulely --- ibuffer settings for emacs
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Keywords: buffer

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
;;  Using Emacs as a multi-file editing tool

;;; Code:

;; Cleanup obsolete buffers automatically
(require 'midnight)

(add-hook 'ibuffer-mode-hook
            (lambda ()
                  (hl-line-mode t))
              't)

(require 'ibuf-ext)
;; (add-to-list 'ibuffer-never-show-predicates "^\\*") ;; 不显示*开头的buffer

(setq ibuffer-modified-char ?m);; 用 m 字符表示 modified 的 buffer
(setq ibuffer-read-only-char ?r);; 用 r 表示只读 buffer

(setq ibuffer-never-show-predicates
      (list
       "^\\*Buffer List\\*$"
       "^\\*CEDET Global\\*$"
       "^\\*MiniBuf-*"
       "^\\*Egg:Select Action\\*$"
       "^\\*Ido Completions\\*$"
       "^\\*SPEEDBAR\\*$"
       "^\\*nav\\*$"
       "^\\*swank\\*$"
       "^\\*slime-events\\*$"
       "^\\*RE-Builder\\*$"
       "^\\*pomodoro\\*$"
       "^\\*Project Buffers\\*$"
       "^eproject$"
       "\\*fsm-debug\\*$"
       ;; "^"
       "^\\*.*\\(-preprocessed\\)\\>\\*"
       "^\\*Calendar\\*$"
       "^\\*ORG.*\\*"
       "^\\*ac-mode-*"
       ".loaddefs.el$"
       "^loaddefs.el$"
       "\\*GTAGS SELECT\\**"
       "\\*Symref*"
       "\\*cscope\\*"
       "\\*helm*"
       "\\*magit*"
       "\\*ag*"
       "\\MusicBox"
       "\\*Completions*"
       "\\*buffer-selection*"
       "\\*mu4e-log*"
       "\\*Flycheck*"
       ))


;; @see http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
;; You’ll still be prompted for confirmation when deleting modified
;; buffers after the option has been turned off.
(setq ibuffer-expert t)

;;Turning off ibuffer-show-empty-filter-groups is particularly useful,
;;because the empty filter groups can really clutter things up.
(setq ibuffer-show-empty-filter-groups nil
      ibuffer-marked-char ?✓)

;; Get rid of title and summary
(setq ibuffer-display-summary nil)
(defadvice ibuffer-update-title-and-summary (after remove-column-titles)
  (with-current-buffer
    (set-buffer "*Ibuffer*")
    (read-only-mode 0)
    (goto-char 1)
    (search-forward "-\n" nil t)
    (delete-region 1 (point))
    (let ((window-min-height 1))
      ;; save a little screen estate
      (shrink-window-if-larger-than-buffer))
    (read-only-mode)))

(ad-activate 'ibuffer-update-title-and-summary)

(defun ibuffer-unmark-all-withour-ret ()
  "Unmark all buffers with mark MARK."
  (interactive)
  (if (= (ibuffer-count-marked-lines t) 0)
      (message "No buffers marked; use 'm' to mark a buffer")
    (ibuffer-map-lines
     #'(lambda (buf mark)
         (when (not (char-equal mark ?\s))
           (ibuffer-set-mark-1 ?\s))
         t)))
  (ibuffer-redisplay t))

;; @see http://rcl.googlecode.com/svn-history/r39/trunk/lian-emacs-lisp/lian-lisps-settings/wuxch-buffer-settings.el
(defun ibuffer-do-delete-then-forward-line ()
  "原有的ibuffer-do-delete关闭一个buffer之后跳到上一个
buffer，不习惯，改为到下一个buffer。"
  (interactive)
  (ibuffer-do-delete)
  (ibuffer-forward-line))

(defun wuxch-ibuffer-update ()
  ""
  (interactive)
  ;; (wuxch-mark-useless-buffer)
  (ibuffer-update nil)
  )

(defun wuxch-ibuffer-compair-marked-buffer ()
  (interactive)
  (let* ((buf (ibuffer-get-marked-buffers))(buf-num (safe-length buf)))
    (cond
     ;; <2 buffers
     ((< buf-num 2)
      (message "please marked 2 or 3 buffers to compare" )
      )
     ;; 2 buffers
     ((eq buf-num 2)
      (ediff-buffers (nth 0 buf) (nth 1 buf))
      )
     ;; 3 buffers
     ((eq buf-num 3)
      (ediff-buffers3 (nth 0 buf) (nth 1 buf) (nth 2 buf))
      )
     ;; others
     (t
      (message "too many marked buffers. only 2 or 3 marked buffers should be compared" ))
     )
    )
  )


;;; provide features
(provide 'ibuffer-mikulely)

;;; ibuffer-mikulely.el ends here
