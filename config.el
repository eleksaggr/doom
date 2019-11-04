;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;; General
(setq user-full-name "Alex Egger")
; When working on big projects this gets old quick, so disable file watchers.
(setq lsp-enable-file-watchers nil)
; Define a helper function to determine which PC we are running on.
(defun my/is-work-host ()
  "Returns whether Emacs is currently running on my PC at work, by comparing hostnames."
  (let ((host (system-name)))
    (and
     (not (string= host "PC"))
     (not (string= host "Laptop"))))
  )
; Set e-mail address based on which PC we are working on.
(setq user-mail-address
      (if (my/is-work-host)
          "alex.egger@mixed-mode.de"
        "alex.egger96@gmail.com"
        ))
(if (my/is-work-host)
    (setq user-mail-address "alex.egger@mixed-mode.de")
  (setq user-mail-address "alex.egger96@gmail.com")
  )

;;; UI
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-theme 'doom-vibrant
      ; Italics looks horrible in code.
      doom-themes-enable-italic nil)

;; C/C++
; Define a style that conforms to the Linux kernel style.
(defun kernel/setup-style ()
  "Sets the required options for the Linux kernel style."
  (setq c-basic-offset 8
        indent-tabs-mode t
        tab-width 8)
  )
(add-hook 'c-mode-common-hook 'kernel/setup-style)

; Taken from: https://github.com/coldnew/linux-kernel-coding-style.el/blob/master/linux-kernel-coding-style.el#L35
(defun kernel/c-lineup-arglist-tabs-only ()
  "Line up arguments by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(c-add-style "kernel" '("linux" (c-offsets-alist
                                 (arglist-cont-nonempty
                                  c-lineup-gcc-asm-reg
                                  kernel/c-lineup-arglist-tabs-only))
                        ))

; Set the default C style to be the kernel style.
(setq c-default-style '((java-mode . "java")
                       (awk-mode . "awk")
                       (other . "kernel")))

;;; Org Mode
(after! org
  ; Enable Org Habits.
  (add-to-list 'org-modules 'org-habit t)
  ; Disable flycheck for Org mode.
  (setq flycheck-global-modes '(not org-mode))

  ; Org Capture Templates
  (defvar org-inbox-file "~/org/inbox.org" "The file the GTD inbox is stored in.")
  (setq org-capture-templates '(
                                ("t" "Todo" entry (file org-inbox-file) "* TODO %?\n" :empty-lines 1)
                                ("l" "Todo with Backlink" entry (file org-inbox-file) "* TODO %?\n%a" :empty-lines 1)
                                ))

  ; Restore the C-c c keybind for org-capture.
  (map! :nvi "C-c c" 'org-capture)
  )

;;; Mail
(defun my/work-mail-setup ()
  "Sets all settings related to my work's e-mail."
  (setq +mu4e-backend 'offlineimap
        mu4e-update-interval 120
        mu4e-attachment-dir "~/Downloads"
        mu4e-compose-dont-reply-to-self t
        mu4e-headers-auto-update t
        mu4e-headers-include-related t
        mu4e-headers-skip-duplicates t
        mu4e-sent-messages-behavior 'delete
        )

  ; Set the update interval.
  (after! mu4e
    (setq mu4e-update-interval 120)
    )

  ; Set configuration for the work e-mail account.
  (set-email-account! "work"
                      '((smtpmail-smtp-server . "localhost")
                        (smtpmail-smtp-service . 1025)
                        (smtpmail-smtp-user . "aleegg")
                        (smtpmail-stream-type . nil)
                        (mu4e-drafts-folder . "/Drafts")
                        (mu4e-refile-folder . "/Archive")
                        (mu4e-sent-folder . "/Sent")
                        (mu4e-trash-folder . "/Trash")
                        (mu4e-compose-signature . t)
                        (message-signature-file . "~/.doom.d/signature.tpl")))
  ; Enable e-mail alerts.
  (setq mu4e-alert-notify-repeated-mails t)

  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  )

(if (my/is-work-host) (my/work-mail-setup))

;;; Utility

(defun fetch-cobie-menu ()
  "Fetches Cobie's menu for today."
  (interactive)
  (with-output-to-temp-buffer "*cobie-menu*"
    (shell-command "lunchlens-exe" "*cobie-menu*")
    (pop-to-buffer "*cobie-menu*")
    (fit-window-to-buffer)
    )
  )
