;;; ~/.doom.d/utility.el -*- lexical-binding: t; -*-

;;; Utility
(defun my/is-work-host ()
  "Return whether Emacs is currently running on my PC at work, by comparing hostnames."
  (let ((host (system-name)))
    (and
     (not (string= host "PC"))
     (not (string= host "Laptop"))))
  )

(defun my/fetch-cobie-menu ()
  "Fetch Cobie's menu for today."
  (interactive)
  (with-output-to-temp-buffer "*cobie-menu*"
    (shell-command "lunchlens-exe" "*cobie-menu*")
    (pop-to-buffer "*cobie-menu*")
    (fit-window-to-buffer)
    )
  )
