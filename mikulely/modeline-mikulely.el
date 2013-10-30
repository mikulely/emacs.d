;;; modeline-mikulely --- modeline settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Created: 2013/06/17 02:33:01
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
;;  Eye Candy For Emacs

;;; Code:


(require-package 'nyan-mode)
(nyan-mode 1)
;; @see http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html

(setq-default
 mode-line-format
 '(
   ;; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "      ")))
   "    "
   ;; relative position, size of file
   "["(:propertize "%I" 'face 'font-lock-constant-face)"] "

   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; narrow [default -- keep?]
   " %n "
   ; mode indicators: vc, recursive edit, major mode,
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "

   ;;global-mode-string, org-timer-set-timer in org-mode need this
   (propertize "%M" 'face 'font-lock-type-face)

   ;; minor modes
   ;; (:eval (propertize (format-mode-line minor-mode-alist)
   ;;                    'face 'mode-line-minor-mode-face))
   ;; process
   (:propertize mode-line-process
                face mode-line-process-face)
   ;; global
   (global-mode-string global-mode-string)
   ;; nyan-mode uses nyan cat as an alternative to %p
   (:eval (when nyan-mode (list (nyan-create))))

   ;; add the time, with the date and the emacs uptime in the tooltip
   " %["
   (:eval (propertize (format-time-string "%H:%M")
                       'help-echo
                       (concat (format-time-string "%c; ")
                               (emacs-uptime "Uptime:%hh"))))
   "%] "
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR"
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
                    :foreground "gray60" :background "gray20"
                    :inverse-video nil
                    :box '(:line-width 4 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray80" :background "gray40"
                    :inverse-video nil
                    :box '(:line-width 4 :color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :background "#ffffff"
                    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face
                    :foreground "gray60")

(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#eab700"
                    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :family "Menlo" :height 120)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "gray40"
                    :height 120)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "black" :background "#eab700")

;;------------------------------------------------------------------------------
;; Change mode-line color by evil state
;;------------------------------------------------------------------------------
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))

  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-normal-state-p)  '("gray20" . "DarkGreen"))
                                 ((evil-visual-state-p) '("DarkOrange2". "ivory"))
                                 ((evil-insert-state-p) '("DarkCyan" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("purple4" . "black"))
                                 ((buffer-modified-p)   '("OrangeRed2" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

;;------------------------------------------------------------------------------
;; Highlight matching parens
;;------------------------------------------------------------------------------
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
;; @see https://github.com/GriffinSchneider/emacs-config/blob/master/init.el
;; Make show-paren-mode use a different color than highlight-parentheses
(set-face-foreground 'show-paren-match "Orange")
(set-face-background 'show-paren-match nil)
(set-face-bold-p 'show-paren-match t)

;; Highlight current line
;; (global-hl-line-mode 1)
; (set-face-background 'hl-line "seashell2")

(add-hook 'package-menu-mode-hook
            (lambda ()
                  (hl-line-mode t))
              't)
;;------------------------------------------------------------------------------
 ;; 设置另外一些颜色：语法高亮显示的背景和主题，区域选择的背景和主题，二次选择的背景和选择
;;------------------------------------------------------------------------------
;; (set-face-foreground 'highlight "white")
;; (set-face-background 'highlight "blue")
;; (set-face-foreground 'region "cyan")
;; (set-face-background 'region "blue")
;; (set-face-foreground 'secondary-selection "skyblue")
;; (set-face-background 'secondary-selection "darkblue")

(setq evil-visual-state-cursor '("white" bar))
(setq evil-insert-state-cursor '("dark orange" bar))
(setq evil-motion-state-cursor '("gray" box))

;; Make window divider line the same color as the fringe
(set-face-foreground 'vertical-border (face-background 'fringe))


;;; provide features
(provide 'modeline-mikulely)


;;; modeline-mikulely.el ends here
