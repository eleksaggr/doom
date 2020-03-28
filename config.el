;;; .doom.d/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path doom-private-dir)

(load "clang.el")
(load "org-settings.el")
(load "utility.el")

(setq user-full-name "Alex Egger")

(setq system-time-locale "C")

;;; UI
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-theme 'doom-oceanic-next
                                        ; Italics looks horrible in code.
      doom-themes-enable-italic nil)

(when (eq doom-theme 'doom-oceanic-next)
  (set-face-attribute 'font-lock-comment-face nil
                      :inherit nil))

;; Spotify
(add-to-list 'load-path "~/.local/share/spotify.el")
(require 'spotify)
(load "spotify-secrets.el")
(define-key spotify-mode-map (kbd "C-c s") 'spotify-command-map)
