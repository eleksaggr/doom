;;; .doom.d/config.el -*- lexical-binding: t; -*-

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
(setq org-todo-keywords '(
                          (sequence "TODO(t)" "|" "DONE(d)")
                          (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)")
                          (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
                          )
      org-refile-targets '(("~/org/projects.org" :maxlevel . 1)
                           ("~/org/someday.org" :maxlevel . 1)
                           )
      org-capture-templates
      '(
        ("t" "Todos")
        ("tt" "Todo" entry (file "~/org/inbox.org") "* TODO %?\n")
        ("tt" "Todo with Reference" entry (file "~/org/inbox.org") "* TODO %? %^G\n%a" :empty-lines 1)
        )
      org-agenda-start-on-weekday 1
      org-ellipsis " ▼ "
      )
