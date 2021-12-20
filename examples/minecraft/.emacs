;; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Kyle R. Burton <kyle.burton@gmail.com>

;;
;; This is my personal emacs configuration.  Check it out into
;; $HOME/personal/projects/krbemacs, then symlink it to $HOME/.emacs.
;;

(require 'cl)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (setq xx-package-archives package-archives)
;; (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
;; (package-refresh-contents)
(loop for package in '(clojure-mode dash pkg-info paredit ido auto-complete
				    align-cljlet rainbow-mode rainbow-delimiters
				    highlight-parentheses color-theme
				    clojure-snippets queue)
      do
      (unless (package-installed-p package)
	(package-install package)))
;; (package-refresh-contents)
;; NB: we use a git checkout of cider
;;(add-to-list 'load-path "software/cider")
(add-to-list 'load-path "~/.emacs.d/local/cider")
(require 'cider)
(require 'cider-inspector)

;; NB: we use a git checkout of ac-cider
;;(add-to-list 'load-path "software/ac-cider")
(add-to-list 'load-path "~/.emacs.d/local/ac-cider")
(require 'ac-cider)
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-mode-hook 'auto-complete-mode)

(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))


(require 'align-cljlet)
(require 'highlight-parentheses)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(ido-mode t)


(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (paredit-mode +1)
	    (setq abbrev-mode t)))


(add-hook 'paredit-mode-hook
          (lambda ()
            (local-set-key "\M-k" 'kill-sexp)))

(defun krb-file-ext-case-permute (pattern)
  "Helper for ading file-extension to editor mode bindings.

  (krb-file-ext-case-permute \"foo\") => (\"foo\" \"FOO\" \"Foo\")"
  (loop for mutator in '(downcase upcase capitalize)
        collect (funcall mutator pattern)))

(defun krb-push-file-ext-and-mode-binding (mode-name &rest patterns)
  "Bind the given mode name to the given set of file
extensions (patterns). Eg:

  (krb-push-file-ext-and-mode-binding 'cperl-mode \"\\.pl$\" \"\\.pm$\" \"\\.al$\")
"
  (dolist (pattern (apply #'append (mapcar #'krb-file-ext-case-permute patterns)))
    (setq auto-mode-alist
          (cons (cons pattern mode-name)
                (remove-if (lambda (elt)
                             (string= (car elt)
                                      pattern))
                           auto-mode-alist)))))

(krb-push-file-ext-and-mode-binding 'clojure-mode "\\.clj$")
(krb-push-file-ext-and-mode-binding 'clojure-mode "\\.cljs$")



(require 'rainbow-delimiters)

(add-hook
 'paredit-mode-hook
 '(lambda ()
    (local-set-key "\M-Oa" 'paredit-splice-sexp-killing-backward)
    (local-set-key "\M-Ob" 'paredit-splice-sexp-killing-forward)
    (local-set-key "\M-Oc" 'paredit-forward-slurp-sexp)
    (local-set-key "\M-Od" 'paredit-forward-barf-sexp)
    (rainbow-delimiters-mode t)
    ;; (rainbow-paren-mode)
    (setq abbrev-mode t)))


(add-hook 'lisp-mode-hook
          (lambda ()
            (paredit-mode +1)
            (setq abbrev-mode t)))

(defvar krb-clojure-replay-expression-expr nil)
(make-variable-buffer-local 'krb-clojure-replay-expression-expr)

(defun krb-clojure-set-replay-expression (expression)
  (interactive
   (list
    (read-string
     ;; prompt
     (concat "Autoeval Expression: " (slime-last-expression) ": ")
     ;; initial-input
     krb-clojure-replay-expression-expr
     ;; history
     'krb-clojure-set-replay-expression-hist
     ;; default-value
     (slime-last-expression)
     ;; inherit-input-method
     t)))
  (if (not (= (length expression) 0))
      (progn
        (message "updating last expression to: %s" expression)
        (setq krb-clojure-replay-expression-expr expression))))

(defun krb-clojure-replay-expression ()
  (interactive)
  (slime-interactive-eval krb-clojure-replay-expression-expr))

(defvar krb-clojure-replay-inspect-expression-expr nil)
(make-variable-buffer-local 'krb-clojure-replay-inspect-expression-expr)

(defun krb-clojure-set-replay-inspect-expression (expression)
  (interactive
   (list
    (read-string
     ;; prompt
     (concat "Autoinspect Expression: " (slime-last-expression) ": ")
     ;; initial-input
     krb-clojure-replay-inspect-expression-expr
     ;; history
     'krb-clojure-set-replay-inspect-expression-hist
     ;; default-value
     (slime-last-expression)
     ;; inherit-input-method
     t)))
  (if (not (= (length expression) 0))
      (progn
        (message "updating last expression to: %s" expression)
        (setq krb-clojure-replay-inspect-expression-expr expression))))

(defun krb-clojure-replay-inspect-expression ()
  (interactive)
  (slime-inspect krb-clojure-replay-inspect-expression-expr))

(add-hook
 'clojure-mode-hook
 (lambda ()
   (paredit-mode +1)
   (setq abbrev-mode t)
   (local-set-key [f6]           'krb-clojure-replay-expression)
   (local-set-key (kbd "C-<f6>") 'krb-clojure-set-replay-expression)
   (local-set-key [f7]           'krb-clojure-replay-inspect-expression)
   (local-set-key (kbd "C-<f7>") 'krb-clojure-set-replay-inspect-expression)))


;;;
;; (require 'rainbow-delimiters)
;; (require 'auto-complete)
;; (require 'auto-complete-config)

(defun silent-auction-cider-connect ()
  (interactive)
  (cider-connect "localhost" "4009"))

(defun silent-auction-realign-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-region (point) (mark))))

(defun silent-auction-fixup-ns ()
  "Ok, eventually this should fixup the entire ns (remove unused imports, resolve new ones, etc).  For now, it aligns the :as and :only forms."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((start (point)))
      (forward-sexp 1)
      (align-regexp start (point) (concat "\\(\\s-*\\)" "\\(:as\\|:refer\\|:only\\)")))))

(defvar silent-auction-cider-mode-prefix-keymap nil)
(setq silent-auction-cider-mode-prefix-keymap
      (let ((map (make-sparse-keymap)))
	;; (define-key map "ss"  'silent-auction-cider-connect)
	(define-key map "a"  'align-cljlet)
	(define-key map "\\" 'silent-auction-realign-buffer)
	(define-key map "fn" 'silent-auction-fixup-ns)
	map))

(defun silent-auction-cider-mode-hook ()
  (interactive)
  (highlight-parentheses-mode t)
  (local-set-key "\C-cr" silent-auction-cider-mode-prefix-keymap))

(add-hook 'clojure-mode-hook 'silent-auction-cider-mode-hook)

(global-set-key "\C-css" 'silent-auction-cider-connect)
(global-set-key "\M-g" 'goto-line)

(defun silent-auction-revert-buffer-cmd ()
  (interactive)
  (revert-buffer nil t)
  (message "<f5> buffer reverted"))

(global-set-key (kbd "<f5>") 'silent-auction-revert-buffer-cmd)
(global-set-key (kbd "<f9>") 'join-line)

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
;; for some reason this isn't applying itself fully if run directly from the .emacs
;; the next few lines are a hack...
;; (color-theme-pok-wob)
(run-with-idle-timer
 0 ;; 0.1 ;; 0.5 ;; 1
 nil
 '(lambda ()
    (message "applying color theme color-theme-pok-wob")
    (color-theme-pok-wob)))

;; White On Black <- this is what I picked for now, the rest are ok too
;; Comidia
;; Dark Laptop
;; Hober
;; Midnight
;; Oswald
;; Taming Mr Arneson
;; TTY Dark


(yas-global-mode 1)
(when (file-exists-p (expand-file-name "~/.emacs.d/snippets"))
    (yas/load-directory (expand-file-name "~/.emacs.d/snippets")))

;; (yas/load-directory "software/clojure-snippets/snippets")



(add-hook 'before-save-hook 'delete-trailing-whitespace)
