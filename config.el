;;; .doom.d/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path doom-private-dir)

(load "clang.el")
(load "org-settings.el")
(load "secrets.el")
(load "utility.el")

(setq user-full-name "Alex Egger")
(setq user-mail-address "alex.egger96@gmail.com")

(setq system-time-locale "C")

;;; UI
(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Open Sans" :size 16)
      display-line-numbers-type nil
      doom-theme 'doom-moonlight
      doom-themes-enable-italic nil ; Italics looks horrible in code.
      doom-themes-treemacs-theme "doom-colors")
(doom-themes-treemacs-config)

;;; Keybindings
(map! "C-c z" #'magit-dispatch)
(map! :nvi "C-c /" #'counsel-projectile-rg)
(map! "C-c d" #'projectile-dired-other-window)

(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
  )
