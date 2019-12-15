;;; .doom.d/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path doom-private-dir)

(load "clang.el")
(load "mail.el")
(load "org-settings.el")
(load "utility.el")

(setq user-full-name "Alex Egger")

;;; UI
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-theme 'doom-moonlight
      ; Italics looks horrible in code.
      doom-themes-enable-italic nil
      doom-modeline-mu4e t)

(when (eq doom-theme 'doom-moonlight)
  (set-face-attribute 'font-lock-comment-face nil
                      :inherit nil)
  (set-face-attribute 'org-todo nil
                      :foreground "turquoise"
                      :background nil)
  (set-face-attribute 'org-done nil
                      :foreground "#696e84"
                      :background nil))
