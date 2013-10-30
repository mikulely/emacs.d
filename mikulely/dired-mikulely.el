;;; dired-mikulely --- dired mode settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Keywords: dired

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
;; Using Emacs As File Manager

;;; Code:

(require-package 'dired-details)

;;------------------------------------------------------------------------------
(require-package 'direx)
(require-package 'popwin)
(require 'direx)
;; If you are using popwin, you can use directory viewer as temporary
;; "side-bar", like this:
(require 'popwin)
(popwin-mode 1)

(push '(direx:direx-mode :position right :width 50 :dedicated t)
            popwin:special-display-config)

;; undo-tree
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)

;; vc
(push "*vc-diff*" popwin:special-display-config)
(push "*vc-change-log*" popwin:special-display-config)

;;------------------------------------------------------------------------------

;; @see http://ergoemacs.org/emacs/emacs_dired_tips.html
;; If you want Enter and ^ (parent dir) to use the same buffer, put the
;; following in your emacs init file:
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "<return>")
              'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))
                                        ; was dired-up-directory
            ))

;; (global-set-key (kbd "C-x d") 'dired-single-magic-buffer)

;; @see http://www.emacswiki.org/emacs/DiredSorting
;; DiredSortDirectoriesFirst
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; @see http://ann77.emacser.com/Emacs/EmacsDiredExt.html
;; Under Emacs-state
;; s s 按照文件大小排序。
;; s x 按照文件扩展名排序。
;; s t 按照文件访问时间排序。
;; s b 按照文件名称的字母顺序排序。

(add-hook 'dired-mode-hook (lambda ()
       (interactive)
       (make-local-variable  'dired-sort-map)
       (setq dired-sort-map (make-sparse-keymap))
       (define-key dired-mode-map "s" dired-sort-map)
       (define-key dired-sort-map "s"
         '(lambda () "sort by Size"
            (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
       (define-key dired-sort-map "x"
         '(lambda () "sort by eXtension"
            (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
       (define-key dired-sort-map "t"
         '(lambda () "sort by Time"
            (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
       (define-key dired-sort-map "n"
         '(lambda () "sort by Name"
            (interactive) (dired-sort-other (concat dired-listing-switches ""))))))

(require 'dired-details)
(require 'dired-x)
(dired-details-install)

;; @see http://ergoemacs.org/emacs/emacs_dired_tips.html
;; Now, go to dired, then call split-window-vertically, then go to another
;; dired dir. Now, when you press C to copy, the other dir in the split
;; pane will be default destination. Same for R (rename; move).
(setq dired-dwim-target t)

;; @see http://blog.binchen.org/?p=575
(defun diredext-exec-git-command-in-shell (command &optional arg file-list)
  "Run a shell command `git COMMAND`' on the marked files.
if no files marked, always operate on current line in dired-mode
"
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "git command on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (setq command (concat "git " command))
  (dired-do-shell-command command arg file-list)
  (message command))

(eval-after-load 'dired
  '(progn
     ;; {dired-details
     (setq-default dired-details-hidden-string "")
     (define-key dired-mode-map "(" 'dired-details-toggle)
     (define-key dired-mode-map ")" 'dired-details-toggle)
     ;; }
     ; (define-key dired-mode-map "%" 'diredext-exec-git-command-in-shell)

     (require 'dired+)
     ;; always delete and copy recursively
     (setq dired-recursive-deletes 'always)
     (setq dired-recursive-copies 'always)
     (define-key dired-mode-map [mouse-2] 'dired-find-file)
     (dolist (file `(("zathura" "pdf" "dvi" "pdf.gz" "ps" "eps")
                     ("unrar x" "rar")
                     ("mplayer -stop-xscreensaver" "avi" "mpg" "rmvb" "rm" "flv" "wmv" "mkv" "mp4" "m4v" "webm")
                     ("mplayer -playlist" "list" "pls")
                     ("feh" "gif" "jpeg" "jpg" "tif" "png" )
                     ("display" "gif" "jpeg" "jpg" "tif" "png")
                     ("7z x" "7z")
                     ("djview" "djvu")
                     ("firefox" "xml" "xhtml" "html" "htm" "mht")))
       (add-to-list 'dired-guess-shell-alist-default
                    (list (concat "\\." (regexp-opt (cdr file) t) "$")
                          (car file))))
     ))

;; @http://whattheemacsd.com/
;; In dired, M-> and M-< never take me where I want to go.
;; now they do
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))


(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

;;; provide features
(provide 'dired-mikulely)

;;; dired-mikulely.el ends here
