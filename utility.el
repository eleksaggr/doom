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
