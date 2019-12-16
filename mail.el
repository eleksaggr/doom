;;; ~/.doom.d/mail.el -*- lexical-binding: t; -*-

(after! mu4e
  (setq +mu4e-backend 'mbsync
        mu4e-change-filenames-when-moving t
        mu4e-compose-dont-reply-to-self t
        mu4e-headers-auto-update t
        mu4e-headers-include-related t
        mu4e-headers-skip-duplicates t
        mu4e-sent-messages-behavior 'delete
        mu4e-update-interval 180
        mu4e-use-fancy-chars t)

  (set-email-account! "Work"
                      '(
                      `(
                        ; Address
                        (user-mail-address . "alex.egger@mixed-mode.de")
                        ; SMTP
                        (smtpmail-smtp-server . "localhost")
                        (smtpmail-smtp-service . 1025)
                        (smtpmail-smtp-user . "aleegg")
                        (smtpmail-stream-type . nil)
                        ; Folders
                        (mu4e-drafts-folder . "/Drafts")
                        (mu4e-refile-folder . "/Archive")
                        (mu4e-sent-folder . "/Sent")
                        (mu4e-trash-folder . "/Trash")
                        ; Signature
                        (mu4e-compose-signature . t)
                        (message-signature-file . "resources/mm.sig")))

  ; Unset most pre-set bookmarks, as I never use them.
  (setq mu4e-bookmarks
        `(,(make-mu4e-bookmark
         :name "Unread Messages"
         :query "flag:unread AND NOT flag:trashed"
         :key ?u))))

(after! mu4e-alert
  (setq mu4e-alert-email-notification-types '(subject))

  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))
