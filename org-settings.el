;;; ~/.doom.d/org.el -*- lexical-binding: t; -*-


(after! org
  (require 'org-mu4e)
  ; Enable Org modules.
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-checklist t)

  (setq org-tags-column -77
        org-tags-exclude-from-inheritance '("project"))

  ; Keywords
  (custom-declare-face '+org-todo-abort '((t (:inherit (bold error org-todo)))) "")
  (custom-declare-face '+org-todo-next '((t (:inherit (bold org-todo) :box 1))) "")
  (custom-declare-face '+org-todo-wait '((t (:inherit (bold warning org-todo)))) "")

  (setq org-todo-keywords '("TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "ABRT(a@)")
        org-todo-keyword-faces '(("ABRT" . +org-todo-abort)
                                 ("NEXT" . +org-todo-next)
                                 ("WAIT" . +org-todo-wait)))

  ; Capture templates
  (defvar org-inbox-file (expand-file-name "inbox.org" org-directory))
  (defvar org-habits-file (expand-file-name "habits.org" org-directory))
  (defvar org-project-file (expand-file-name "projects.org" org-directory))

  (setq org-capture-templates '(("t" "Todo" entry (file org-inbox-file) "* TODO %?\n")
                                ("r"
                                 "Respond to Email"
                                 entry (file org-inbox-file)
                                 "* TODO Respond to %:fromname on \"%:subject\" :@email:
%a
Received on: %U" :empty-lines 1 :immediate-finish t)))

  ; Agenda
  (setq org-agenda-compact-blocks nil
        org-agenda-dim-blocked-tasks nil
        org-agenda-span 7
        org-agenda-start-day nil
        org-agenda-window-setup 'other-window)
  ; Swap the default definition of a stuck project to something more useful.
  (setq org-stuck-projects '("*" ("NEXT") nil nil))

  (defvar org-agenda-block-view-span 3)
  (setq org-agenda-custom-commands
        '(
          ("b" "Block View"
           ((agenda "" ((org-agenda-span org-agenda-block-view-span)))
            (tags-todo "/!+NEXT"
                       ((org-agenda-overriding-header "Next Tasks")
                        (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))
                        (org-tags-match-list-sublevels t)))
            (tags-todo "-refile-STYLE=\"habit\"/!-WAIT-NEXT"
                       ((org-agenda-overriding-header "Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
            (tags-todo "-refile-STYLE=\"habit\"/!+WAIT"
                       ((org-agenda-overriding-header "Tasks on Hold")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
            (tags "refile"
                  ((org-agenda-overriding-header "Tasks to Refile")))
            (tags "-ARCHIVE/DONE|ABRT"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))))
          ))

  ; FIX: Align tags when entering `org-capture-mode'.
  (add-hook 'org-capture-mode-hook #'org-align-all-tags)
  ; FIX: Switch into evil insert mode when adding a note to a task.
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state)

  ; Keybindings
  (map! :nvi "C-c a" #'org-agenda)
  (map! :nvi "C-c c" #'org-capture)
  )
