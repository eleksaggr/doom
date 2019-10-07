;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; User
(setq user-mail-address "alex.egger@mixed-mode.de"
      user-full-name "Alex Egger")

;; Font
(if (string= (system-name) "Laptop")
  (setq doom-font (font-spec :family "Iosevka" :size 16))
  (setq doom-font (font-spec :family "Iosevka" :size 14))
)

;; Theme
(setq doom-theme 'doom-dracula
      doom-themes-enable-italic nil)

;; Pretty Code
(setq +pretty-code-enabled-modes '(haskell-mode))

;; Org-Mode
(after! org
  (setq org-todo-keywords '(
                            (sequence "TODO(t)" "|" "DONE(d)")
                            (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)")
                            (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
                            ))
  (setq org-refile-targets '(("~/org/projects.org" :maxlevel . 1)
                            ("~/org/someday.org" :maxlevel . 1)
                            ))
  (setq org-capture-templates
        '(
          ("t" "Todos")
          ("tt" "Todo" entry (file "~/org/inbox.org") "* TODO %?\n")
          ("tr" "Todo with Reference" entry (file "~/org/inbox.org") "* TODO %? %^G\n%a" :empty-lines 1)
          ("te" "Todo from Email" entry (file "~/org/inbox.org") "* TODO %?\n%a" :empty-lines 1)
          ("r" "Report")
          ;; ("rw" "Weekly Report" entry (file (expand-file-name (format-time-string "weekly-%V.org") org-directory))
          ;;  (file "~/org/weekly.tpl"))
          ))
  (setq org-agenda-start-on-weekday 1)
  (setq org-ellipsis " ▼ ")
  (require 'org-mu4e)
)
;; Mail
(setq +mu4e-backend 'offlineimap
      mu4e-update-interval 120
      )

(set-email-account! "mixed-mode.de"
                    '((mu4e-sent-folder . "/Sent")
                      (mu4e-drafts-folder . "/Drafts")
                      (mu4e-trash-folder . "/Trash")
                      (mu4e-refile-order . "/Archive")
                      (smtpmail-smtp-server . "localhost")
                      (smtpmail-smtp-service . 1025)
                      (smtpmail-smtp-user . "aleegg")
                      (smtpmail-stream-type . nil)
                      (mu4e-compose-signature . "Mixed Mode GmbH
Lochhamer Schlag 17
D-82166 Gräfelfing/München
Tel.	+49 / 89 / 89 86 8-200
Fax	+49 / 89 / 89 86 8-222
http://www.mixed-mode.de

Mixed Mode - Ihr Partner für Embedded & Software Engineering!

-------

Ein Unternehmen der PIXEL Group

Mixed Mode GmbH
Sitz der Gesellschaft: Gräfelfing
Amtsgericht: München
HRB 130778
Geschäftsführer: Helmut Süßmuth, Paul Privler

-------

Diese E-Mail enthält vertrauliche und/oder rechtlich geschützte Informationen.
Wenn Sie nicht der richtige Adressat sind oder diese E-Mail irrtümlich erhalten haben, informieren Sie bitte sofort den Absender und vernichten Sie diese Mail.
Das unerlaubte Kopieren sowie die unbefugte Weitergabe dieser Mail sind nicht gestattet.
Über das Internet versandte Emails können leicht unter fremden Namen erstellt oder manipuliert werden.
Aus diesem Grunde bitten wir um Verständnis, dass wir zu Ihrem und unserem Schutz die rechtliche Verbindlichkeit der vorstehenden Erklärungen und Äußerungen ausschließen."))
                    t)

(after! mu4e-alert
  'mu4e-alert-enable-notifications)
