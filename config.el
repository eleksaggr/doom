;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Font
(setq doom-font (font-spec :family "Iosevka" :size 14))

;; Theme
(setq doom-theme 'doom-dracula
      doom-themes-enable-italic nil)

;; Pretty Code
;; Disable pretty-code in Python mode
(setq +pretty-code-enabled-modes '(not python-mode))
