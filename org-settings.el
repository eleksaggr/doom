;;; ~/.doom.d/org.el -*- lexical-binding: t; -*-

(defun my/org-add-modules ()
  "Add needed modules to `org-modules'."
  (add-to-list 'org-modules 'org-checklist t)
  (add-to-list 'org-modules 'org-habit t))

(defun my/org-declare-faces ()
  "Declare faces for `org-mode' todo keywords."
  (custom-declare-face '+org-todo-abort '((t (:inherit (bold error org-todo)))) "")
  (custom-declare-face '+org-todo-next '((t (:inherit (bold org-todo) :box 1))) "")
  (custom-declare-face '+org-todo-wait '((t (:inherit (bold warning org-todo)))) ""))

(defun my/org-setup-todo-keywords ()
  "Setup todo keywords."
  (setq org-todo-keywords '("TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "ABRT(a@)")
        org-todo-keyword-faces '(("ABRT" . +org-todo-abort)
                                 ("NEXT" . +org-todo-next)
                                 ("WAIT" . +org-todo-wait))))

(defun my/org-setup-tags ()
  "Set tag-related settings."
  (setq org-tags-column -77
        org-tags-exclude-from-inheritance '("project")))

(defun my/org-setup-capture-templates ()
  "Setup capture templates."
  (setq org-capture-templates '(("t" "Todo" entry (file org-inbox-file) "* TODO %?\n")
                                ("r"
                                 "Respond to Email"
                                 entry (file org-inbox-file)
                                 "* TODO Respond to %:fromname on \"%:subject\" :@email:
%a
Received on: %U" :empty-lines 1 :immediate-finish t)))
  )

(defun my/org-setup-agenda ()
  "Setup agenda."
  (setq org-agenda-compact-blocks nil
        org-agenda-dim-blocked-tasks 'invisible
        org-agenda-show-all-dates nil
        org-agenda-span 7
        org-agenda-start-day nil
        org-agenda-window-setup 'other-window)
                                        ; Swap the default definition of a stuck project to something more useful.
  (setq org-stuck-projects '("+project/-ABRT-DONE" ("NEXT") nil nil))

  (setq org-agenda-custom-commands
        '(
          ("b" "Block Agenda"
           ((agenda "" ((org-agenda-span 5)))
            (tags-todo "-someday-training/!+NEXT"
                       ((org-agenda-overriding-header "Next Tasks")
                        (org-agenda-sorting-strategy '(effort-up category-up))
                        (org-tags-match-list-sublevels t)))
            (tags-todo "-someday-training-refile-STYLE=\"habit\"/!-WAIT-NEXT"
                       ((org-agenda-overriding-header "Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down effort-up category-up))))
            (tags-todo "-someday-training-refile-STYLE=\"habit\"/!+WAIT"
                       ((org-agenda-overriding-header "Tasks on Hold")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down effort-up category-up))))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")))
            (tags "someday"
                  ((org-agenda-overriding-header "Unscheduled/Undecided Tasks")
                   (org-agenda-max-entries 10)
                   (org-agenda-sorting-strategy '(todo-state-down effort-up category-up))))
            (tags "refile"
                  ((org-agenda-overriding-header "Tasks to Refile")))
            (tags "-ARCHIVE/DONE|ABRT"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-sorting-strategy '(todo-state-down effort-up category-up))))))
          ))
  )

(after! org
  (require 'org-mu4e)

  (defvar org-archive-dir (concat org-directory (file-name-as-directory "archive")))
  (defvar org-inbox-file (expand-file-name "inbox.org" org-directory))
  (defvar org-someday-file (expand-file-name "someday.org" org-directory))
  (defvar org-project-file (expand-file-name "projects.org" org-directory))

  (my/org-add-modules)

  (my/org-declare-faces)
  (my/org-setup-todo-keywords)

  (setq org-refile-targets '((org-inbox-file :level . 1)
                             (org-project-file :maxlevel . 3)
                             (org-someday-file :level . 1)))

  ; Store archives in an archive subfolder
  (setq org-archive-location (format "%s%%s_archive::" org-archive-dir))

  (my/org-setup-tags)
  (my/org-setup-capture-templates)
  (my/org-setup-agenda)

                                        ; FIX: Align tags when entering `org-capture-mode'.
  (add-hook 'org-capture-mode-hook #'org-align-all-tags)
                                        ; FIX: Switch into evil insert mode when adding a note to a task.
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state)

                                        ; Keybindings
  (map! :nvi "C-c a" #'org-agenda)
  (map! :nvi "C-c c" #'org-capture)

  (add-hook 'org-mode-hook (lambda ()
                             (when (eq doom-theme 'doom-moonlight)
                               (set-face-attribute 'org-todo nil
                                                   :foreground "turquoise"
                                                   :background nil)
                               (set-face-attribute 'org-done nil
                                                   :foreground "#696e84"
                                                   :background nil)))))
