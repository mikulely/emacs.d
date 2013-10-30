;;; util-mikulely --- usefull elisp functions collection
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Created: 2013/06/14 04:01:08
;; Keywords: function

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
;;  Usefull Function Collection

;;; Code:

;; @see https://github.com/jhamrick/emacs/blob/master/.emacs.d/settings/custom-functions.el
(defun unfill-paragraph ()
  "Unfill a paragraph, i.e., make it so the text does not wrap in the paragraph where the cursor is."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  "Unfill a region, i.e., make is so the text in that region does not wrap."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun youtube ()
  "Search YouTube with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))

(defun encode2utf8 ()
  "Sets the buffer-file-coding-system to UTF8."
  (interactive)
  (set-buffer-file-coding-system 'utf-8 nil))

(defun dos2unix ()
  "convert a buffer from dos ^M end of lines to unix end of lines"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  "convert a buffer from unix end of lines to dos ^M end of lines"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(defun clone-file-and-open (filename)
  "Clone the current buffer writing it into FILENAME and open it"
  (interactive "FClone to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm))
  (find-file filename))

(defun set-file-executable()
  "Add executable permissions on current file."
  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))
;;------------------------------------------------------------------------------
;; http://www.emacswiki.org/emacs/DuplicateLines
;;------------------------------------------------------------------------------
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun untabify-this-buffer ()
  "Indent whole buffer and delete trailing whitespace.
This command will also do untabify."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (message "Untabify this buffer successfullly!")
  )
;;------------------------------------------------------------------------------
;; Frame Handling
;;------------------------------------------------------------------------------
(defun window-horizontal-to-vertical ()
  "Switches from a horizontal split to a vertical split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

(defun window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

(defun rotate-windows-helper (x d)
  "Helper function for rotate window."
  (if (equal (cdr x) nil)
      (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x)))
    (rotate-windows-helper (cdr x) d)))

(defun rotate-windows ()
  "Rotate window but make the focus on the same bufer."
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

(defun rotate-windows-only ()
  "Rotate windows only, keep focus at the same window."
  (interactive)
  ;; sotre window
  (let ((cur (car (window-list))))
    (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
    (select-window cur)))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun scratch-toggle ()
  "Toggle between *scratch* buffer and the current buffer.
   If the *scratch* buffer does not exist, create it."
  (interactive)
  (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
    (if (equal (current-buffer) scratch-buffer-name)
        (switch-to-buffer (other-buffer))
      (progn
        (switch-to-buffer scratch-buffer-name)
        (unless (equal major-mode 'lisp-interaction-mode)
          (lisp-interaction-mode))))))

;; @http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
(defun now ()
  "Insert string for the current time formatted like ' 2013-10-10 2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d %-I:%M %p")))

(defun today ()
  "Insert string for today's date nicely formatted in American style, 2013-10-10."
  (interactive)           
  ;; Thursday, October 17, 2013  -> "%A, %B %e, %Y"
  (insert (format-time-string "%Y-%m-%d")))

(defun inc-num-region (p m)
  "将给定区域中的重复数字替换为递增数字."
  (interactive "r")
  (save-restriction
    (save-excursion
      (narrow-to-region p m)
      (goto-char (point-min))
      (forward-line)
      (let ((counter 1))
        (while (not (eq (point)
                        (point-max)))
          (goto-char (point-at-eol))
          (search-backward-regexp "[0-9]+" (point-at-bol) t)
          (let* ((this-num (string-to-number (match-string 0)))
                 (new-num-str (number-to-string (+ this-num
                                                   counter))))
            (replace-match new-num-str)
            (incf counter)
            (forward-line)))))))


(defun sort-lines-random (beg end)
  "sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; to make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

;; @see https://gist.github.com/dbrady/846766
;; camelcase-region Given a region of text in snake_case format,
;; changes it to camelCase.
(defun camelcase-region (start end)
  "Changes region from snake_case to camelCase"
  (interactive "r")
  (save-restriction (narrow-to-region start end)
                    (goto-char (point-min))
                    (while (re-search-forward "_\\(.\\)" nil t)
                      (replace-match (upcase (match-string 1))))))

;; cadged largely from http://xahlee.org/emacs/elisp_idioms.html:
;;
(defun camelcase-word-or-region ()
  "Changes word or region from snake_case to camelCase"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (camelcase-region pos1 pos2)))

;; snakecase-region Given a region of text in camelCase format,
;; changes it to snake_case.
;;
;; BUG: This is actually just a repeat of camelcase-region!
(defun snakecase-region (start end)
  "Changes region from camelCase to snake_case"
  (interactive "r")
  (save-restriction (narrow-to-region start end)
                    (goto-char (point-min))
                    (while (re-search-forward "_\\(.\\)" nil t)
                      (replace-match (upcase (match-string 1))))))

;; Given a region of text in camelCase format, changes it to
;; snake_case.
(defun snakecase-word-or-region ()
  "Changes word or region from camelCase to snake_case"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (snakecase-region pos1 pos2)))

;;------------------------------------------------------------------------------
;; Edit Files as Root
;; @see http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
;;------------------------------------------------------------------------------
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))



(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (prelude-ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))
      (recenter)))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (prelude-ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names (substring-no-properties name))
          (add-to-list 'name-and-pos (cons (substring-no-properties name) position))))))))


;;; provide features
(provide 'util-mikulely)

;;; util-mikulely.el ends here
