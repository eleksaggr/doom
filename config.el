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
  ; Restore the C-c c keybind for org-capture.
  (map! :nvi "C-c c" 'org-capture))
