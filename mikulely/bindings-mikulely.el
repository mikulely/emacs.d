;;; bindings-mikulely --- emacs key bindings settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Created: 2013/06/1 02:16:37
;; Keywords: evil-mode, global key map

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
;; Using Emacs As Super Vim

;;; Code:

(require-package 'ace-jump-mode)
(require-package 'evil)
(require-package 'surround)
(require-package 'evil-leader)

;;------------------------------------------------------------------------------
;; Evil requires undo-tree.el in the load-path for linear undo and undo branches.
;; Otherwise, Evil uses regular Emacs undo.
;;------------------------------------------------------------------------------
(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(global-surround-mode 1)

;;------------------------------------------------------------------------------
;; Note: You should enable global-evil-leader-mode before you enable evil-mode,
;; otherwise evil-leader won’t be enabled in initial buffers (*scratch*, *Messages*,
;; … ).
;;------------------------------------------------------------------------------
(global-evil-leader-mode)
(evil-mode 1)


(require-package 'evil-numbers)
(define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)
;;------------------------------------------------------------------------------
;; Using Evil mode in Emacs results in the cursor color staying black
;; no matter what theme you use. In order to change the cursor color
;; add the following to your config
;;------------------------------------------------------------------------------
(setq evil-default-cursor t)


(require-package 'evil-numbers )
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt )
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt )
;;------------------------------------------------------------------------------
;; ESC quits
;;------------------------------------------------------------------------------
(define-key  evil-normal-state-map           [escape] 'keyboard-quit)
(define-key  evil-visual-state-map           [escape] 'keyboard-quit)
(define-key  minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
(define-key  minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
(define-key  minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key  minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key  minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)

;;------------------------------------------------------------------------------
;; Keybindings
;; global keymap
;;------------------------------------------------------------------------------
(setq woman-fontify t) ;; 對 woMan 緩衝區上色
(setq woman-use-topic-at-point nil)
;; Colorful fonts
(setq woman-fill-column 100)

(global-set-key [(f1)] (lambda()        ;;设定F1为woman快捷键
                         (interactive)
                         (let ((woman-topic-at-point t))
                           (woman))))

;; The following lines are always needed.  Choose your own keys.
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)

;; 为了正常切换输入法
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key (kbd "C-s")  'save-buffer)
(global-set-key (kbd "M-a")  'mark-whole-buffer)

;; @see https://github.com/dandavison/emacs-config/blob/master/emacs.org
;;
(when (executable-find "ag")
      (require-package 'ag)
         (require-package 'wgrep-ag)
(setq-default ag-highlight-search t)
(setq ag-reuse-window t)
(setq ag-reuse-buffers t))

(require 'ag)
(setq ag-arguments (append '("--ignore" "'*#'"
                             "--ignore" "'*.js'"
                             "--ignore" "'*.xml'"
                             "--ignore" "'*.log'"
                             "--ignore" "'*.sql'"
                             "--ignore" "'*.txt'"
                             "--ignore" "'*.json'"
                             "--ignore" "'*.yaml'"
                             "--word-regexp"
                             )
                           ag-arguments))

(add-hook
 'ag-mode-hook
 (lambda () (switch-to-buffer "*ag*")
   (delete-other-windows)
   ))

(global-set-key (kbd "<f5>") 'ag-project)
(global-set-key (kbd "<f6>") 'ag-regexp-project-at-point)
(global-set-key (kbd "<f9>") 'ibuffer)


(global-set-key (kbd "C-p") 'helm-cmd-t)
(define-key evil-normal-state-map (kbd "C-p") 'helm-cmd-t)
;;------------------------------------------------------------------------------
;; @see  http://nschum.de/src/emacs/window-numbering-mode/
;; window-numbering-mode assigns a number to each window in a Emacs frame,
;; so you can reach any window with just one command (M-1 … M-0)
;;------------------------------------------------------------------------------
(require-package 'window-numbering)
(window-numbering-mode 1)

;;------------------------------------------------------------------------------
;; Terminal at Your Fingertips
;; @see http://emacsredux.com/blog/2013/03/29/terminal-at-your-fingertips/
;;------------------------------------------------------------------------------
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(global-set-key (kbd "C-c t") 'visit-term-buffer)

;;------------------------------------------------------------------------------
;; Instant Access to init.el
;; @see http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
;;------------------------------------------------------------------------------
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(global-set-key (kbd "C-c I") 'find-user-init-file)

;;------------------------------------------------------------------------------
;; Refactor Funcation
;;------------------------------------------------------------------------------
(defun evilcvn-change-symbol-in-defun ()
  "Mark the region in defun (definition of function) and use
string replacing UI in evil-mode to replace the symbol under
cursor."
  (interactive)
  (let ((old (thing-at-point 'symbol)))
    (mark-defun)
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-ex (concat "'<,'>s/" (if (= 0 (length old)) "" "\\<\\(") old (if (= 0 (length old)) "" "\\)\\>/"))))
  )

(global-set-key (kbd "C-c s") 'evilcvn-change-symbol-in-defun)

;;------------------------------------------------------------------------------
;; Open File in External Program
;; @see http://emacsredux.com/blog/2013/03/27/open-file-in-external-program/
;;------------------------------------------------------------------------------
(defun prelude-open-with (arg)
  "Open visited file in default external program.
    With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))

(global-set-key (kbd "C-c o") 'prelude-open-with)

;;------------------------------------------------------------------------------
;; Code Folding
;;------------------------------------------------------------------------------
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

;;------------------------------------------------------------------------------
;; visual-state keymap
;;------------------------------------------------------------------------------
(define-key evil-visual-state-map "|"  'align-regexp)

;;------------------------------------------------------------------------------
;; @see http://www.masteringemacs.org/articles/2011/03/16/removing-blank-lines-buffer/
;;------------------------------------------------------------------------------
(defun flush-blank-lines (start end)
  "删除选中区域空行。"
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

(define-key evil-visual-state-map "-"  'flush-blank-lines)

;;------------------------------------------------------------------------------
(defun google-this-region ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(define-key evil-visual-state-map "?" 'google-this-region)

;;------------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(define-key evil-visual-state-map "!" 'sort-lines-random)

;;------------------------------------------------------------------------------
;; 快速打开常用文件
;; @see http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html
;;------------------------------------------------------------------------------
(defvar bookmark-list nil "alist for files i need to open frequently.
 Key is a short abbrev, Value is file path.")
(setq bookmark-list
      '(
        ("blog"        . "~/Dropbox/octopress/source/blog")
        ("linux-tips"  . "~/Dropbox/octopress/source/blog/2009-12-01-linux-command-tips.org")
        ("org-tips"    . "~/Dropbox/octopress/source/blog/2013-04-19-org-mode-essential.org")
        ("emacs-tips"  . "~/Dropbox/octopress/source/blog/2013-01-02-emacs-tips.org")
        ("Note-org"    . "~/Dropbox/org/note/")
        ("home"        . "~")
        ("dropbox"     . "~/Dropbox")
        ("download"    . "~/Downloads/")))

(defun open-file-fast (openCode)
  "Prompt to open a file from a pre-defined set."
  (interactive
   (list (ido-completing-read "Open:" (mapcar (lambda (x) (car x)) bookmark-list))))
  (find-file (cdr (assoc openCode bookmark-list))))

(define-key evil-normal-state-map "M" 'open-file-fast)

;;------------------------------------------------------------------------------
;; Copy Filename to the Clipboard
;; @see http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
;;------------------------------------------------------------------------------
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(define-key evil-normal-state-map "Y" 'copy-file-name-to-clipboard)

;;------------------------------------------------------------------------------
;; normal-state keymap
;;------------------------------------------------------------------------------
;; Navigation
(define-key evil-normal-state-map "H"         'evil-first-non-blank)
(define-key evil-normal-state-map "L"         'evil-last-non-blank)
(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-item)
(define-key evil-visual-state-map (kbd "TAB") 'evil-jump-item)

;; Buffer switch
(define-key evil-normal-state-map "b"         'ibuffer)

(require-package 'ace-jump-buffer)
(define-key evil-normal-state-map "B"         'ace-jump-buffer)

;; Shift lines up and down
(require-package 'move-text)
(move-text-default-bindings)
(define-key evil-normal-state-map (kbd "C-j") 'move-text-down)
(define-key evil-normal-state-map (kbd "C-k") 'move-text-up)

(define-key evil-normal-state-map (kbd "C-d")  'helm-dired-recent-dirs-view)
(define-key evil-normal-state-map (kbd "C-w")  'delete-trailing-whitespace)
(define-key evil-normal-state-map "K" 'kill-this-buffer)
(define-key evil-normal-state-map ";" 'smex)
(define-key evil-visual-state-map ";" 'smex)

;;------------------------------------------------------------------------------
;; AceJump Integration
;; see http://www.emacswiki.org/emacs/Evil
;;------------------------------------------------------------------------------
(defmacro evil-enclose-ace-jump (&rest body)
  `(let ((old-mark (mark))
         (ace-jump-mode-scope 'window))
     (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
     (remove-hook 'post-command-hook #'evil-visual-post-command t)
     (unwind-protect
         (progn
           ,@body
           (recursive-edit))
       (if (evil-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
             (add-hook 'post-command-hook #'evil-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

(evil-define-motion evil-ace-jump-char-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 5)))

(evil-define-motion evil-ace-jump-line-mode (count)
  :type line
  (evil-enclose-ace-jump
   (ace-jump-mode 9)))

(evil-define-motion evil-ace-jump-word-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 1)))

(evil-define-motion evil-ace-jump-char-to-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 5)
   (forward-char -1)))

(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

(define-key evil-normal-state-map   (kbd "SPC")   'evil-ace-jump-word-mode)
(define-key evil-operator-state-map (kbd "SPC")   'evil-ace-jump-word-mode)

;; different jumps for different visual modes
(defadvice evil-visual-line (before spc-for-line-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))
(defadvice evil-visual-char (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))
(defadvice evil-visual-block (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

;;------------------------------------------------------------------------------
;;Manage Processes With Proced
;; @see http://emacsredux.com/blog/2013/05/02/manage-processes-with-proced/
;;------------------------------------------------------------------------------
(define-key evil-normal-state-map "X" 'proced)

;;------------------------------------------------------------------------------
;; emacs-state keymap
;;------------------------------------------------------------------------------
(evil-declare-key 'emacs undo-tree-mode-map
  "j"         'undo-tree-visualize-undo
  "k"         'undo-tree-visualize-redo
  ;; 最爱的 t 看时间
  ;; 分支之前 b、f 左右切换
  )

;;------------------------------------------------------------------------------
;; insert-state keymap
;; 将 Emacs 模式绑定在 Insert 模式下
;;------------------------------------------------------------------------------
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;; 因为覆盖了 insert-stat 的绑定，所以我们需要再绑定 ESC
(define-key evil-insert-state-map [escape] 'evil-normal-state)

(evil-declare-key 'emacs douban-music-mode-map
  "r"         'douban-music-refresh
  "b"         'ibuffer
  "i"         'douban-music-current-song-info
  "j"         'evil-next-line
  "k"         'evil-previous-line
  "h"         'evil-forward-word-begin
  "l"         'evil-backward-word-begin
  "M"         'open-file-fast
  )

(evil-declare-key 'emacs info-mode-map
  "j"         'evil-next-line
  "k"         'evil-previous-line
  "h"         'evil-forward-word-begin
  "l"         'evil-backward-word-begin
  )

;; magit related
(evil-define-key 'normal magit-log-edit-mode-map "q" 'magit-log-edit-commit)
(evil-declare-key 'emacs magit-mode-map
  ";" 'magit-toggle-section
  ;; Map "j" to magit-goto-next-section in eveywhere
  ;; @see https://github.com/GriffinSchneider/emacs-config/blob/master/init.el
  "j" (lambda () (interactive)
        (let ((next (magit-find-section-after (point))))
          (if next
              (magit-goto-section next)
            (goto-char (+ -1 (magit-section-end (magit-current-section)))))))
  "k" 'magit-goto-previous-section
  "K" 'magit-discard-item
  )

(evil-declare-key 'emacs magit-branch-manager-mode-map
  "K" 'magit-discard-item
  "k" 'magit-goto-previous-section
  )

(evil-declare-key 'emacs git-log-view-mode-map
  "j"  'evil-next-line
  "k"  'evil-previous-line
  )

(evil-declare-key 'normal ibuffer-mode-map
  " "         'ibuffer-mark-forward
  "r"         'ibuffer-update
  "b"         'ibuffer-quit
  "|"         'bookmark-bmenu-list
  (kbd "* n") 'ibuffer-mark-by-name-regexp
  (kbd "* m") 'ibuffer-mark-by-mode
  (kbd "D")   'ibuffer-do-delete-then-forward-line
  (kbd "U")   'ibuffer-unmark-all-withour-ret
  "f"         'helm-for-files
  ;; (kbd "=")   'wuxch-ibuffer-compair-marked-buffer
  (kbd "n")   'ibuffer-forward-filter-group
  (kbd "p")   'ibuffer-backward-filter-group
  )

(evil-declare-key 'emacs direx:direx-mode-map
  "j"  'next-line
  "k"  'previous-line
  )
(evil-declare-key 'normal dired-mode-map
  "b"         'ibuffer
  "f"         'dired-jump
  " "         'dired-mark
  "."         'diredp-mark/unmark-extension
  ";"         'smex
  "/"         'evil-search-forward
  "r"         'revert-buffer
  "n"         'evil-search-next
  "N"         'evil-search-previous
  "p"         'emms-play-dired
  "g"         'dired-back-to-top
  (kbd "C-g") 'dired-jump-to-bottom
  (kbd "C-a") 'gnus-dired-attach
  )

(evil-declare-key 'emacs compilation-mode-map
  "j"  'evil-next-line
  "k"  'evil-previous-line
  )

(evil-declare-key 'emacs emms-playlist-mode-map
  (kbd "b")   'ibuffer
  (kbd "j")   'next-line
  (kbd "k")   'previous-line
  (kbd "x")   'emms-start
  (kbd "h")   'emms-shuffle
  (kbd "o")   'emms-show
  (kbd "SPC") 'emms-pause
  (kbd "r")   'emms-toggle-repeat-track
  (kbd "R")   'emms-toggle-repeat-playlist
  (kbd "q")   'winner-undo
  (kbd "p")   'emms-previous
  (kbd "n")   'emms-next
  "-"         'emms-volume-lower
  "="         'emms-volume-raise
  "/"         'mikulely-emms-search
  )
(evil-declare-key 'emacs compilation-mode-map
  "j"  'evil-next-line
  "k"  'evil-previous-line
  )

(evil-declare-key 'emacs package-menu-mode-map
  "j"  'next-line
  "k"  'previous-line
  "b"  'ibuffer
  "/"  'evil-search-forward
  "n"  'evil-search-next
  "N"  'evil-search-previous
  )
(evil-declare-key 'emacs cfw:calendar-mode-map
  "H" 'cfw:navi-goto-week-begin-command
  "L" 'cfw:navi-goto-week-end-command
  )

(evil-declare-key 'emacs mu4e-headers-mode-map
  "j"         'next-line
  "k"         'previous-line
  "G"         'evil-goto-line
  (kbd "C-j") 'mu4e~headers-jump-to-maildir
  (kbd "C-d") 'mu4e-headers-mark-thread
  )

(evil-declare-key 'normal mu4e-view-mode-map
  "j"         'next-line
  "k"         'previous-line
  "J"         'mu4e-view-headers-next
  "K"         'mu4e-view-headers-prev
  "C"         'mu4e-compose-new
  "D"         'mu4e-view-mark-for-delete
  "-"         'mu4e-headers-split-view-shrink
  "="         'mu4e-headers-split-view-grow
  "q"         'mu4e~view-quit-buffer
  "R"         'mu4e-compose-reply
  (kbd "C-g") 'mu4e-view-go-to-url
  (kbd "C-j") 'mu4e~headers-jump-to-maildir
  (kbd "C-k") 'mu4e-select-other-view
  (kbd "C-d") 'mu4e-view-mark-thread
  (kbd "C-s") 'mu4e-view-save-attachment
  )

(evil-declare-key 'emacs mu4e-view-mode-map
  "j"         'next-line
  "k"         'previous-line
  "J"         'mu4e-view-headers-next
  "K"         'mu4e-view-headers-prev
  "C"         'mu4e-compose-new
  "D"         'mu4e-view-mark-for-delete
  "-"         'mu4e-headers-split-view-shrink
  "="         'mu4e-headers-split-view-grow
  "q"         'mu4e~view-quit-buffer
  "R"         'mu4e-compose-reply
  (kbd "C-g") 'mu4e-view-go-to-url
  (kbd "C-j") 'mu4e~headers-jump-to-maildir
  (kbd "C-k") 'mu4e-select-other-view
  (kbd "C-d") 'mu4e-view-mark-thread
  (kbd "C-s") 'mu4e-view-save-attachment
  )

(evil-declare-key 'emacs speedbar-mode-map
  "r"             'sr-speedbar-refresh-toggle
  "q"             'sr-speedbar-toggle
  "j"             'next-line
  "k"             'previous-line
  "."             'speedbar-up-directory
  (kbd "<right>") 'speedbar-flush-expand-line
  (kbd "<left>")  'speedbar-contract-line
  )

;; org-mode settings
(evil-declare-key 'normal org-mode-map
  "gh" 'outline-up-heading
  "gl" 'outline-next-visible-heading
  ;; 链接操作
  "ml" 'org-insert-link
  "?"  'org-open-at-point
  ;; 大纲操作
  "mh" 'org-ctrl-c-star ;将本行设为标题 / 正文
  "<" 'org-metaleft ; out-dent
  ">" 'org-metaright ; indent
  ;; TODO 应该跳到 * 之后的空格上才算智能
  "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
  "L" 'org-end-of-line ; smarter behaviour on headlines etc.
  ;; 表格
  "mt" 'org-table-create-or-convert-from-region
  ;; 添加水平分割线并跳到下一行
  "m-" 'org-ctrl-c-ret
  ;; 在表格中加入一行分割线，在列表中改变列表的标记
  "-" 'org-ctrl-c-minus
  ;; 按列排序
  "ms" 'org-sort
  ;; C-hjkl 移到表格的行或者列。在列表中则是调整列表项级别
  (kbd "C-l") 'org-metaright
  (kbd "C-h") 'org-metaleft
  (kbd "C-j") 'org-metadown
  (kbd "C-k") 'org-metaup
  ;; 创建行
  "mr" 'org-shiftmetadown
  ;; 创建列
  "mc" 'org-shiftmetaright
  ;; 删除列
  "qc" 'org-shiftmetaleft
  "mi" (lambda () (interactive) (org-meta-return) (evil-insert-state))
  "mb" (lambda () (interactive) (insert "- [ ] ") (evil-insert-state))
  ;; 插入带复选框的项或者 TODO项
  ;; 改变复选框状态
  "t" 'org-todo ; 将一个条目变成 todo 之类的
  ",t" 'org-set-tags-command ;这则是给条目添加 tags
  ",e" 'org-export-dispatch
  ",n" 'outline-next-visible-heading
  ",p" 'outline-previous-visible-heading
  )

(mapc (lambda (state)
        (evil-declare-key 'normal org-mode-map
          (kbd "TAB") 'org-cycle
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-L") 'org-shiftmetaright
          (kbd "M-H") 'org-shiftmetaleft
          (kbd "M-K") 'org-shiftmetaup
          (kbd "M-J") 'org-shiftmetadown))
      '(normal insert))

;;------------------------------------------------------------------------------
;; evil-leader config
;;------------------------------------------------------------------------------
(setq evil-leader/leader "`" evil-leader/in-all-states t)

;; Quick and dirty code folding
;; @see http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun jao-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    ))

(defun indent-this-buffer ()
  "Indent whole buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max)))
  )

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (delete-trailing-whitespace (region-beginning) (region-end))
          (indent-region (region-beginning) (region-end))
          (untabify (region-beginning) (region-end))
          (message "Indent selected region successfullly!."))
      (progn
        (indent-this-buffer)
        (message "Indent this buffer successfullly!.")))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocation toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun lookup-word-definition ()
  "Look up the current word's definition in a browser.
If a region is active (a phrase), lookup that phrase."
  (interactive)
  (let (myword myurl)
    (setq myword
          (if (and transient-mark-mode mark-active)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))

    (setq myword (replace-regexp-in-string " " "%20" myword))
    (setq myurl (concat "http://dict.youdao.com/search?q=" myword "&ue=utf8&keyfrom=dict.index"))

    (browse-url myurl)
    ;; (w3m-browse-url myurl) ;; if you want to browse using w3m
    ))

(evil-leader/set-key
  ;; projectile
  ;; My most used functions are projectile-find-file, projectile-ack, projectile-recentf and projectile-dired.
  ;; C-c p f Display a list of all files in the project. With a prefix argument it will clear the cache first.
  ;; C-c p T Display a list of all test files (specs, features, etc) in the project.
  ;; C-c p g Run grep on the files in the project.
  ;; C-c p b Display a list of all project buffers currently open.
  ;; C-c p o Runs multi-occur on all project buffers currently open.
  ;; C-c p r Runs interactive query-replace on all files in the projects.
  ;; C-c p i Invalidates the project cache (if existing).
  ;; C-c p R Regenerates the projects TAGS file.
  ;; C-c p k Kills all project buffers.
  ;; C-c p d Opens the root of the project in dired.
  ;; C-c p e Shows a list of recently visited project files.
  ;; C-c p a Runs ack on the project. Requires the presence of ack-and-a-half.
  ;; C-c p l Runs a standard compilation command for your type of project.
  ;; C-c p p Runs a standard test command for your type of project.
  ;; C-c p z Adds the currently visited to the cache.
  ;; C-c p s Display a list of known projects you can switch to.
  ;; calendar
  "io" 'cfw:open-calendar-buffer
  ;; jump
  "jw" 'switch-window
  ;; email
  "em"  'mu4e-up-to-date-status
  "ec"  'compose-mail
  ;; music
  "mm"  'emms-play-directory-tree
  "ml"  'emms-playlist-mode-go
  "mr"  'emms-toggle-repeat-track
  "mR"  'emms-toggle-repeat-playlist
  "mp"  'emms-previous
  "mn"  'emms-next
  "ms"  'emms-pause
  "mq"  'emms-stop
  "mi"  'emms-show
  ;; 代码整理
  "="  'indent-region-or-buffer
  "ov" 'jao-toggle-selective-display
  "c"  'comment-or-uncomment-region-or-line
  ;; highlight-indentation
  "hi" 'highlight-indentation-mode
  "hg" 'highlight-indentation-current-column-mode
  ;; magit
  "gc" 'magit-checkout
  "gs" 'magit-status
  ;; find sth
  "fm" 'lookup-word-definition
  "fg" 'helm-google-suggest
  "fo" 'helm-occur
  "ff" 'ffap
  "fs"  'helm-imenu
  "8"   'cpplint
  "C"  'smart-compile
  "w"  'save-some-buffers
  ;; gtags
  "tf"  'helm-gtags-find-tag
  "tr"  'helm-gtags-find-rtag
  "ts"  'helm-gtags-find-symbol
  "tb"  'helm-gtags-pop-stack
  ;; quick back
  "b" 'switch-to-previous-buffer
  ;; dired-x.el 提供了两个函数可以用于跳转到当前 buffer 所对应的文件的目录
  "d"  'dired-jump-other-window
  "." 'direx:jump-to-directory-other-window
  )

;;------------------------------------------------------------------------------
;; Use tab to move between links in help mode.
(evil-define-key 'motion help-mode-map (read-kbd-macro "TAB") 'forward-button)

;;------------------------------------------------------------------------------
;; Initial State Settings
;;------------------------------------------------------------------------------
(loop for (mode . state) in
      '(
        (douban-music-mode          . emacs)
        (pianobar-mode              . emacs)
        (comint-mode                . emacs)
        (erc-mode                   . emacs)
        (magit-log-edit-mode        . emacs)
        (magit-commit-mode          . emacs)
        (magit-branch-manager-mode  . emacs)
        (Info-mode                  . emacs)
        (term-mode                  . emacs)
        (log-edit-mode              . emacs)
        (inf-ruby-mode              . emacs)
        (yari-mode                  . emacs)
        (gud-mode                   . emacs)
        (help-mode                  . emacs)
        (eshell-mode                . emacs)
        (shell-mode                 . emacs)
        (message-mode               . emacs)
        (gtags-select-mode          . emacs)
        (weibo-timeline-mode        . emacs)
        (weibo-post-mode            . emacs)
        (diff-mode                  . emacs)
        (sr-mode                    . emacs)
        (compilation-mode           . emacs)
        (helm-grep-mode             . emacs)
        (speedbar-mode              . emacs)
        (mu4e-main-mode             . emacs)
        (mu4e-headers-mode          . emacs)
        (mu4e-about-mode            . emacs)
        (mu4e-view-mode             . emacs)
        (calendar-mode              . emacs)
        (direx:direx-mode           . emacs)
        (undo-tree-mode             . emacs)
        (inferior-racket-mode       . emacs)
        (fundamental-mode           . normal)
        (text-mode                  . normal)
        (ibuffer-mode               . normal)
        (ag-mode                    . normal)
        )
      do (evil-set-initial-state mode state))

(require-package 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-force-normal-state)

;;; provide features
(provide 'bindings-mikulely)

;;; bindings-mikulely.el ends here
