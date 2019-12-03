;;; ~/.doom.d/utility.el -*- lexical-binding: t; -*-

;;; Utility
(defun my/is-work-host ()
  "Return whether Emacs is currently running on my PC at work, by comparing hostnames."
  (let ((host (system-name)))
    (and
     (not (string= host "PC"))
     (not (string= host "Laptop"))))
  )


;;; Style
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

;;; Org
(defun my/is-project-p ()
  "Whether a task is a project (has atleast one subtask)."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun my/find-project-task ()
  "Move point to the project task."
  (save-restriction
    (widen)
    (let ((parent (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent (point))))
      (goto-char parent)
      parent)))

(defun my/is-project-subtree-p ()
  "Whether a task is a subtree of a project."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
    (save-excursion (my/find-project-task)
                    (if (equal (point) task) nil t))))

(defun my/skip-stuck-projects ()
  "Skip trees that are stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (my/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun my/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
   (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (my/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun my/skip-non-projects ()
  "Skip trees that are not projects."
  (if (save-excursion (my/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond ((my/is-project-p) nil)
                ((and (my/is-project-subtree-p) (my/is-project-p)) nil)
                (t subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun my/skip-non-tasks ()
  "Skip trees that are non-project tasks."
  (save-restriction
    (widen)
    (let ((next (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((not (or (my/is-project-p) (my/is-project-subtree-p))) nil)
       (t next)))))

;;; Various
(defun my/fetch-cobie-menu ()
  "Fetch Cobie's menu for today."
  (interactive)
  (with-output-to-temp-buffer "*cobie-menu*"
    (shell-command "lunchlens-exe" "*cobie-menu*")
    (pop-to-buffer "*cobie-menu*")
    (fit-window-to-buffer)
    )
  )
