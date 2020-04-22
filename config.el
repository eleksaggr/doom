;;; .doom.d/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path doom-private-dir)

(load "clang.el")
(load "org-settings.el")
(load "secrets.el")
(load "utility.el")

(setq user-full-name "Alex Egger")

(setq system-time-locale "C")

;;; UI
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-theme 'doom-gruvbox
                                        ; Italics looks horrible in code.
      doom-themes-enable-italic nil)

(when (eq doom-theme 'doom-oceanic-next)
  (set-face-attribute 'font-lock-comment-face nil
                      :inherit nil))
;;; Dired
(setq dgi-auto-hide-details-p nil)
(add-hook 'dired-after-readin-hook #'dired-git-info-auto-enable)

;;; Keybindings
(map! :nvi "C-z" #'magit-dispatch)
(map! :nvi "C-c /" #'counsel-projectile-rg)
(map! :nvi "C-c d" #'projectile-dired-other-window)
