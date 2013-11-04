;;; emms-mikulely --- EMMS Settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Keywords: music

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
;; Using Emacs As MusicBox

;;; Code:
(require-package 'emms)

(require 'emms-setup)
(require 'emms-volume)
(emms-standard)
(emms-default-players)

(add-hook 'emms-playlist-mode-hook
          (lambda ()
            (hl-line-mode t))
          't)

;; Show the current track each time EMMS
;; starts to play a track with "播放 : "
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "正在播放: %s")

;; When asked for emms-play-directory,
;; always start from this one
(if (not (file-exists-p (expand-file-name "~/.emacs.d/emms")))
    (make-directory (expand-file-name "~/.emacs.d/emms")))

;; 默认的播放目录
(setq emms-source-file-default-directory "~/Dropbox/Music/")
(setq emms-playlist-buffer-name "MusicBox")

(require 'emms-playing-time)
(emms-playing-time 1)
(emms-playing-time-mode-line)

;;------------------------------------------------------------------------------
;; helper function
;;------------------------------------------------------------------------------
(defun mikulely-emms-search ()
  "Emms: search in the browser or play-list"
  (interactive)
  (goto-char (point-min))
  (call-interactively 'isearch-forward))

;;; provide features
(provide 'emms-mikulely)

;;; emms-mikulely.el ends here
