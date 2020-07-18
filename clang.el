;;; ~/.doom.d/clang.el -*- lexical-binding: t; -*-

; Linux Kernel Coding Style
(c-add-style "kernel" `("linux"
                        (c-basic-offset . 8)
                        (indent-tabs-mode . t)
                        (tab-width . 8)
                        (c-offsets-alist
                         (arglist-cont-nonempty
                          c-lineup-gcc-asm-reg
                          ,(lambda ()
                             (let* ((anchor (c-langelem-pos c-syntactic-element))
                                    (column (c-langelem-2nd-pos c-syntactic-element))
                                    (offset (- (1+ column) anchor))
                                    (steps (floor offset c-basic-offset)))
                               (* (max steps 1) c-basic-offset)))))))

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "kernel")))

; Disable CCLS re-indexing the whole kernel every time.
; Also disable LSP file watchers for the kernel.
(add-hook 'c-mode-hook
          (lambda ()
            (let* ((filename (buffer-file-name))
                   (path (expand-file-name filename))
                   (found nil))
              (while (and path (not found))
                (if (file-exists-p "Kconfig")
                    (setq ccls-initialization-options '(:index (:initialBlacklist))
                          lsp-enable-file-watchers nil
                          found t)
                  (setq path (file-name-directory path))))
              (unless found (setq ccls-initialization-options nil
                                  lsp-enable-file-watchers t)))))
