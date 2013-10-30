;;; mu4e-mikulely --- mu4e settings
;; Copyright (C) 2013 Jiaying Ren

;; Author: Jiaying Ren <mikulely@gmail.com>
;; Created: 2013/06/14 04:01:08
;; Keywords: mu4e, email user agent

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
;;  Using Emacs as Email User Agent

;;; Code:

;;------------------------------------------------------------------------------
;; @see https://github.com/magnars/.emacs.d/blob/master/setup-mu4e.el
;;------------------------------------------------------------------------------
(require 'mu4e)

;; general settings
(setq mail-user-agent 'mu4e-user-agent                   ; mu4e as default mail agent
      mu4e-get-mail-command "offlineimap"                ; fetch email with offlineimap
      mu4e-update-interval 300                           ; update every 5 minutes
      mu4e-confirm-quit nil                              ; don't ask me to quit
      mu4e-headers-skip-duplicates t                     ; skip duplicate email, great for gmail
      mu4e-headers-date-format "%d %b, %H:%M"            ; date format
      mu4e-headers-leave-behavior 'apply                 ; don't prompt for applying of marks, just apply
      mu4e-html2text-command "html2text -width 80" ; convert HTML to text
      ;; mu4e-html2text-command "w3m -dump -T text/html"    ; convert HTML to text
      mu4e-compose-dont-reply-to-self t                  ; don't reply to myself
      message-kill-buffer-on-exit t                      ; don't keep message buffers around
      smtpmail-queue-mail nil                            ; start in non queue mode
      )

;; Try to display images in mu4e
(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 800)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; mu4e uses the xdg-open by default
(setq mu4e-attachment-dir
      (lambda (fname mtype)
        (cond
         ((and fname (string-match "\\.\\([dD][oO][cC]\\|[pP][dD][fF]\\|[xX][mM][lL]\\|[xX][lL][sS]\\)$"  fname)) "~/Documents/")
         ((and fname (string-match "\\.\\([jP][pP][eE]?[gG]\\|[pP][nN][gG]\\|[gG][iI][fF]\\|[bB][mM][pP]\\)$" fname)) "~/Pictures")
         ((and fname (string-match "\\.\\([mM][pP]3\\|[fF][lL][aA][cC]\\|[aA][pP][eE]\\|[wW][mM][aA]\\|[mM][pP]4\\)$"  fname)) "~/Music")
         ((and fname (string-match "\\.\\([rR][mM]?[vV][bB]\\|[v][V][oO][bB]\\|[aA][vV][iI]\\|[dD][vV][dD]\\)$"  fname)) "~/Videos")
         ((and fname (string-match "\\.\\(el\\|sh\\|perl\\|py\\|[cC]\\|[cC][pP][pP]\\|[jJ][aA][vV][aA]\\|[hH][sS]\\|[tT][xX][tT]\\)$"  fname))  "~/sourcecode")
         ;; other cases
         (t "~/attachments"))))

;; Eye Candy
;; @see https://groups.google.com/forum/?fromgroups#!msg/mu-discuss/LaHd4gfsek4/sZ4d9XNeepoJ
(setq mu4e-use-fancy-chars t
      mu4e-headers-draft-mark     '("D" . "⚒ ")  ; draft
      mu4e-headers-seen-mark      '("S" . "☑ ")  ; seen
      mu4e-headers-unread-mark    '("u" . "☐ ")  ; unseen
      mu4e-headers-flagged-mark   '("F" . "⚵ ") ; flagged
      mu4e-headers-new-mark       '("N" . "✉ ") ; new
      mu4e-headers-replied-mark   '("R" . "↵ ")  ; replied
      mu4e-headers-passed-mark    '("P" . "⇉ ")  ; passed
      mu4e-headers-encrypted-mark '("x" . "⚷ ")  ; encrypted
      mu4e-headers-signed-mark    '("s" . "✍ ")) ; signed

;;------------------------------------------------------------------------------
;; @see http://www.djcbsoftware.nl/code/mu/mu4e/Longer-configuration.html
;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
(setq mu4e-headers-fields
      '((:date    . 15)
        (:flags   . 6)
        (:subject . nil)
        (:from    . 20)
        ))

;;------------------------------------------------------------------------------
;; Maildir Folders
;; @see http://emacs-fu.blogspot.com/2012/10/mu4e-v099-is-out.html
;; rebuild的命令 mu index --rebuild --maildir=~/.Mail
;;------------------------------------------------------------------------------
(setq mu4e-maildir       "~/.Mail")  ;; top-level Maildir
(setq mu4e-sent-folder   "/Sent")    ;; where do i keep sent mail?
(setq mu4e-drafts-folder "/Drafts")  ;; where do i keep half-written mail?
(setq mu4e-trash-folder  "/Deleted") ;; where do i move deleted mail?
(setq smtpmail-queue-dir "/Queue")

;; Let the message be line-wrapped
(add-hook 'mu4e-view-mode-hook 'longlines-mode)

;;------------------------------------------------------------------------------
;; Sending Mail
;;------------------------------------------------------------------------------
(setq mu4e-user-mail-address-list
      (list "renjiaying@intra.nsfocus.com"
            "mikulely@gmail.com"
            ))

(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      )

;; @see http://www.emacswiki.org/emacs/GnusMSMTP
(defun cg-feed-msmtp ()
  "Fill msmtp account."
  (if (message-mail-p)
      (save-excursion
        (let* ((from
                (save-restriction
                  (message-narrow-to-headers)
                  (message-fetch-field "from")))
               (account
                (cond
                 ;; I use email address as account label in ~/.msmtprc
               ((string-match "mikulely@gmail.com" from) "mikulely@gmail.com")
                 ;; Add more string-match lines for your email accounts
               ((string-match "renjiaying@intra.nsfocus.com" from) "renjiaying@intra.nsfocus.com")
                 )))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))
 ; the original form of this script did not have the ' before "a"
 ; which causes a very difficult to track bug --frozencemetery

(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)

;;------------------------------------------------------------------------------
;; Compose New Email
;; @see https://github.com/wunki/wunki-dotfiles/blob/master/emacs/wunki/mu4e.el
;;------------------------------------------------------------------------------
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (fci-mode t)
            )
          't)

(setq wunki-mu4e-account-alist
      '(
        ("nsfocus"
         (user-full-name    "任家英")
         (user-mail-address "renjiaying@intra.nsfocus.com")
         (message-signature
          (concat
           "任家英\n"
           "研发一部 ESD 组\n"
           "分机号 5484\n"
           ))
         (mu4e-sent-folder   "/Raw/Nsfocus/Sent")
         (mu4e-drafts-folder "/Raw/Nsfocus/Drafts")
         (mu4e-trash-folder  "/Raw/Nsfocus/Trashs")
         )
        ("mikulely"
         (user-full-name     "mikulely")
         (user-mail-address  "mikulely@gmail.com")
         (message-signature  "Best Regards!\n mikulely\n")
         (mu4e-sent-folder   "/Raw/Mikulely/[Gmail].Sent Mail")
         (mu4e-drafts-folder "/Raw/Mikulely/[Gmail].Drafts")
         (mu4e-trash-folder  "/Raw/Mikulely/[Gmail].Trashs")
         )
        ))

(defun wunki-mu4e-set-account ()
  "Set the account for composing a message by looking at the maildir."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-msg-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) wunki-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) wunki-mu4e-account-alist)
                             nil t nil nil (caar wunki-mu4e-account-alist))))
         (account-vars (cdr (assoc account wunki-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars))))
(add-hook 'mu4e-compose-pre-hook 'wunki-mu4e-set-account)

;;------------------------------------------------------------------------------
;; Reply
;; @see http://zmalltalker.com/linux/mu.html
;; When replying to an email I want to use the address I received this
;; message to as the sender of the reply.
;;------------------------------------------------------------------------------
(add-hook 'mu4e-compose-pre-hook
          (defun my-set-from-address ()
            "Set the From address based on the To address of the original."
            (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
              (if msg
                  (setq user-mail-address
                        (cond
                         ((mu4e-message-contact-field-matches msg :to "renjiaying@intra.nsfocus.com")
                          "renjiaying@intra.nsfocus.com" )
                         ((mu4e-message-contact-field-matches msg :to "mikulely@gmail.com")
                          "mikulely@gmail.com")
                         (t "mikulely@gmail.com")
                         ))))))

;;------------------------------------------------------------------------------
;; With this, I can attach a file as an attachment to a new email message
;; by entering C-c RET C-a, and I'm good to go.
;;------------------------------------------------------------------------------
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(defun mu4e-up-to-date-status ()
  "Start mu4e in fullscreen, immediately ping for new email."
  (interactive)
  (window-configuration-to-register :mu4e-fullscreen)
  (mu4e)
  (delete-other-windows))

;; (global-set-key (kbd "C-x M") 'mu4e-up-to-date-status)
;; 绑在evil-mode下的'`e'上

;;------------------------------------------------------------------------------
;; Restore previous window configuration
;;------------------------------------------------------------------------------
(defun mu4e-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :mu4e-fullscreen))

(define-key mu4e-main-mode-map (kbd "q") 'mu4e-quit-session)

;;------------------------------------------------------------------------------
;; bookmark
;; @see http://ionrock.org/emacs-email-and-mu.html
;;------------------------------------------------------------------------------
(setq mu4e-bookmarks
      '(
        ("subject:Word of the day*"                "Words"                  ?v)
        ("date:today..now"                         "Today's Messages"       ?t)
        ("date:7d..now"                            "Last 7 Days"            ?w)
        ("mime:*"                                  "Attachments"            ?a)
        ("mime:image/*"                            "Images"                 ?i)
        ("flag:unread AND NOT flag:trashed"        "Unread Messages"        ?u)
        ("maildir:/Raw/Mikulely/INBOX"             "Mikulely Inbox"         ?g)
        ("maildir:/Raw/Nsfocus"                    "Nsfocus Inbox"          ?n)
        ))

;;; message view action
(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the MSG in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))


(add-to-list 'mu4e-view-actions
             '("View in browser" . mu4e-msgv-action-view-in-browser) t)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

;;; provide features
(provide 'mu4e-mikulely)

;;; mu4e-mikulely.el ends here
