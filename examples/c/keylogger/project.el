;;; project.el --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Emacs configuration for local project
;;;
;;; Code:
(require 'eglot)
(require 'c-mode)

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  ;; (define-key eglot-mode-map (kbd "M->") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-?") 'xref-find-references)

  (define-key eglot-mode-map (kbd "M-,") 'xref-go-back))

(setq c-macro-preprocessor "cpp")

(provide 'local-project)
;;; project.el ends here
