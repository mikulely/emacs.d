;;; abbrev-mikulely --- define personal abbreviations
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Keywords: abbrev

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
;; abbrev mode -- abbreviation
;; Using Emacs Abbrev Mode for Abbreviation
;; @see http://ergoemacs.org/emacs/emacs_abbrev_mode.html

;;; Code:

(define-abbrev-table 'global-abbrev-table '(

    ;; math/unicode symbols
    ("8in"   "∈")
    ("8nin"  "∉")
    ("8inf"  "∞")
    ("8lv"   "♥")
    ("8x"    "≧◔◡◔≦")
    ("加油"  "ᕙ(`▽´)ᕗ 加油～～")
    ("发呆"  "≧☉_☉≦....")
    ("呃"    "⁀⊙﹏☉⁀ ...呃")
    ("hehe"  "ლ(╹◡╹ლ) ")

    ;; computing tech
    ("8wp"   "Wikipedia")
    ("8g"    "Google")
    ("8qt"   "QuickTime")
    ("8win"  "Windows")
    ("8mma"  "Mathematica")
    ("8js"   "javascript")
    ("8yt"   "YouTube")
    ("8ff"   "Firefox")
    ("8e"    "Emacs")

    ;; normal english words
    ("8alt"   "alternative")
    ("8char"  "character")
    ("8def"   "definition")
    ("8bg"    "background")
    ("8kb"    "keyboard")
    ("8ex"    "example")
    ("8kbd"   "keybinding")
    ("8env"   "environment")
    ("8var"   "variable")
    ("8ev"    "environment variable")
    ("8pc"    "computer")
    ("8in"    "Internationalization")

    ;; sig
    ("8jy"    "Jiaying Ren")
    ("8ns"    "renjiaying@intra.nsfocus.com")
    ("8gm"    "mikulely@gmail.com")
    ("8ya"     "renjiaying@ymail.com")

    ;; url
    ("8bl"     "http://mikulely.github.io")

    ;; Emacs Regex
    ("8num"    "\\([0-9]+?\\)")
    ("8str"    "\\([^\"]+?\\)\"")
    ("8curly"  "“\\([^”]+?\\)”")
    ("8wlwn"   ".* \([0-9,]+\).*")

    ;; shell commands
    ;; ("8ditto" "ditto -ck --sequesterRsrc --keepParent src dest")
    ;; ("8im" "convert -quality 85% ")
    ;; ("8ims" "convert -size  -quality 85% ")
    ;; ("8im256" "convert +dither -colors 256 ")
    ;; ("8imf" "find . -name \"*png\" | xargs -l -i basename \"{}\" \".png\" | xargs -l -i  convert -quality 85% \"{}.png\" \"{}.jpg\"")

    ;; ("8f0" "find . -type f -empty")
    ;; ("8f00" "find . -type f -size 0 -exec rm {} ';'")
    ;; ("8chmod" "find . -type f -exec chmod 644 {} ';'")
    ;; ("8chmod2" "find . -type d -exec chmod 755 {} ';'")

    ))

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)

;;; provide features
(provide 'abbrev-mikulely)

;;; abbrev-mikulely.el ends here

