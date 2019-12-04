;;; .doom.d/config.el -*- lexical-binding: t; -*-

(load-file ".doom.d/utility.el")

(defun battery-update ()
  (ignore))

;;; General
(setq user-full-name "Alex Egger")

; Disable flycheck for Org mode.
(setq flycheck-global-modes '(not org-mode))

; Set e-mail address based on which PC we are working on.
(setq user-mail-address
      (if (my/is-work-host)
          "alex.egger@mixed-mode.de"
        "alex.egger96@gmail.com"
        ))

;;; UI
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-theme 'doom-laserwave
      ; Italics looks horrible in code.
      doom-themes-enable-italic nil
      doom-modeline-mu4e t)

;;; LSP
; When working on big projects this gets old quick, so disable file watchers.
(setq lsp-enable-file-watchers nil)

;; C/C++
; Set the default C style to be the kernel style.
(setq c-default-style '((java-mode . "java")
                       (awk-mode . "awk")
                       (other . "kernel")))

; Stop CCLS from indexing the whole project.
; This is really slow when working on the kernel.
(setq ccls-initialization-options '(:index (:initialBlacklist ,"[\".\"]")))

;;; Org Mode
(defun my/org-setup-keywords ()
  "Set up custom org mode keywords and faces."
  (custom-declare-face '+org-todo-wait '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face '+org-todo-abort '((t (:inherit (bold error org-todo)))) "")
  (custom-declare-face '+org-todo-next '((t (:inherit (bold org-todo) :box 1))) "")

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "ABRT(a@)"))
        org-todo-keyword-faces '(("ABRT" . +org-todo-abort)
                                 ("WAIT" . +org-todo-wait)
                                 ("NEXT" . +org-todo-next)))
  )

(defvar org-inbox-file "~/org/inbox.org" "The org file the GTD inbox is stored in.")
(defvar org-projects-file "~/org/projects.org" "The org file GTD projects are stored in.")
(defvar org-habits-file "~/org/habits.org" "The org file habits are stored in.")

(defun my/org-setup-capture-templates ()
  "Set up custom capture templates."
  (require 'org-mu4e)
  (setq org-capture-templates '(
                                ("t" "Todo" entry (file org-inbox-file) "* TODO %?\n")
                                ("l" "Todo with Backlink" entry (file org-inbox-file) "* TODO %?\n%a" :empty-lines 1)
                                ("r" "Respond to Email" entry (file org-inbox-file) "* TODO Respond to %:fromname on %:subject\n%a\nReceived on: %U" :empty-lines 1 :immediate-finish t)
                                ))
  )

(defun my/org-setup-agenda ()
  "Set up org agenda."
  (setq org-agenda-window-setup 'other-window
        org-agenda-span 7
        org-agenda-start-day nil
        org-agenda-show-all-today t
        org-agenda-start-on-weekday 1
        org-agenda-dim-blocked-tasks nil
        org-agenda-compact-blocks nil
        org-stuck-projects '("" nil nil "")
        )

  (setq org-agenda-custom-commands
        '(
          ("b" "Block View"
           ((agenda "" ((org-agenda-span 1)))
            (tags-todo "/!+NEXT"
                       ((org-agenda-overriding-header "Next Tasks")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
            ;; (tags-todo "/!"
            ;;            ((org-agenda-overriding-header "Projects")
            ;;             (org-agenda-skip-function 'my/skip-non-projects)
            ;;             (org-tags-match-list-sublevels t)
            ;;             (org-agenda-sorting-strategy '(category-keep))))
            ;; (tags-todo "/!"
            ;;            ((org-agenda-overriding-header "Stuck Projects")
            ;;             (org-agenda-skip-function 'my/skip-non-stuck-projects)
            ;;             (org-tags-match-list-sublevels t)
            ;;             (org-agenda-sorting-strategy '(category-keep))))
            (tags-todo "-refile-STYLE=\"habit\"/!-WAIT-NEXT-ABRT"
                       ((org-agenda-overriding-header "Tasks")
                        ;; (org-agenda-skip-function 'my/skip-non-tasks)
                        (org-agenda-todo-ignore-with-date t)
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
            (tags-todo "/!+WAIT"
                       ((org-agenda-overriding-header "Tasks on Hold")
                        (org-agenda-todo-ignore-with-date t)
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-sorting-strategy '(category-keep))))
            (tags "refile"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags "-ARCHIVE/DONE|ABRT"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-skip-function 'my/skip-non-tasks)
                   (org-agenda-sorting-strategy '(category-keep))))
            )
           )
          ("w" "Weekly Review"
           ((agenda "" (
                        (org-agenda-span 7)
                        (org-habit-show-habits nil)
                        ))
            (tags "refile"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "/!"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'my/skip-non-stuck-projects)
                        (org-agenda-sorting-strategy '(category-keep))
                        (org-tags-match-list-sublevels t)
                        ))
            (tags "someday"
                  ((org-agenda-overriding-header "Unscheduled Tasks")
                   (org-tags-match-list-sublevels nil)))
            )
           )
          )
        )
  )

(after! org
  ; Enable Org modules.
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-checklist t)

  (my/org-setup-keywords)
  (my/org-setup-capture-templates)
  (my/org-setup-agenda)

  ; Restore default org tags column.
  (setq org-tags-column -77)

  ; Exclude the project tag from inheritance.
  (setq org-tags-exclude-from-inheritance '("project"))

  ; Add hook to update projects if they are marked NEXT.
  (add-hook 'org-after-todo-state-change-hook #'my/raise-parent-task-if-next)

  ; Switch to evil insert mode when adding note.
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state)

  ; Restore the C-c c keybind for org-capture.
  (map! :nvi "C-c c" 'org-capture)
  ; Define shortcut to jump to org-inbox-file directly.
  (map! :nvi "C-c i" (lambda () (interactive) (find-file org-inbox-file)))
  ; Define shortcut to jump to org-projects-file directly.
  (map! :nvi "C-c o" (lambda () (interactive) (find-file org-projects-file)))
  ; Define a handy org-agenda shortcut.
  (map! :nvi "C-c a" 'org-agenda)
  )

;;; Mail
; Enable e-mail alerts.

(mu4e-alert-set-default-style 'libnotify)
(setq mu4e-alert-email-notification-types '(subject))
(setq mu4e-alert-notify-repeated-mails t)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

(defun my/work-mail-setup ()
  "Sets all settings related to my work's e-mail."

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

  )

(after! mu4e
  (setq +mu4e-backend 'mbsync
        mu4e-update-interval 120
        mu4e-compose-dont-reply-to-self t
        mu4e-headers-auto-update t
        mu4e-headers-include-related t
        mu4e-headers-skip-duplicates t
        mu4e-sent-messages-behavior 'delete
        mu4e-change-filenames-when-moving t
        )

  ; Set custom bookmarks.
  (add-to-list 'mu4e-bookmarks
              (make-mu4e-bookmark
                :name "Flagged Messages"
                :query "flag:flagged"
                :key ?f)
              t)

  )

(when (my/is-work-host) (my/work-mail-setup))
