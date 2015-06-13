(require 'cl)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(loop for package in '(clojure-mode dash pkg-info paredit ido auto-complete
				    align-cljlet rainbow-mode rainbow-delimiters
				    highlight-parentheses color-theme
				    clojure-snippets)
      do
      (unless (package-installed-p package)
	(package-install package)))

;; NB: we use a git checkout of cider
(add-to-list 'load-path "~/.emacs.d/krb/cider")
(require 'cider)
(require 'cider-inspector)

;; NB: we use a git checkout of ac-cider
(add-to-list 'load-path "~/.emacs.e/krb/ac-cider")
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
(yas/load-directory ".emacs.d/snippets")

(defun krb-ensure-buffer-exists (buffer-name)
  (if (not (get-buffer buffer-name))
      (save-excursion
        (switch-to-buffer buffer-name))))

(defun krb-ins-into-buffer (buffer-name text)
  (krb-ensure-buffer-exists buffer-name)
  (save-excursion
    (set-buffer buffer-name)
    (insert text)))

(defun krb-insf-into-buffer (buffer-name &rest args)
  (krb-ins-into-buffer buffer-name (apply 'message args)))

(defun krb-clear-buffer (buffer-name)
  (interactive "bBuffer: ")
  (if (get-buffer buffer-name)
      (save-excursion
        (set-buffer buffer-name)
        (goto-char (point-max))
        (kill-region 1 (point)))))

(defmacro krb-with-fresh-output-buffer (buffer-name &rest body)
  `(let ((*krb-buffer-name* ,buffer-name)
         ;; prevents it from additionally being displayed in a minibuffer when the output is small
         (max-mini-window-height 0))
     (when (get-buffer ,buffer-name)
       (message "krb-with-fresh-output-buffer: killing buffer: %s" ,buffer-name)
       (kill-buffer ,buffer-name))
     (krb-clear-buffer ,buffer-name)
     ,@body
     (save-excursion
       (pop-to-buffer ,buffer-name))))

(defun krb-git-grep-do-grep (starting-dir cmd)
  (krb-with-fresh-output-buffer
   "*git-output*"
   (krb-insf-into-buffer "*git-output*" "Executing: %s\n" cmd)
   (save-excursion
     ;; TODO: factor out most of this into something like
     ;; krb-with-fresh-output-buffer: the make-local-variable for
     ;; the output base directory, the use of the buffer name, the
     ;; pop-to-buffer and the binding of the jump key (since these
     ;; are all 'output' temporary buffers)
     (pop-to-buffer "*git-output*")
     (shell-command cmd "*git-output*")
     (goto-char (point-min))
     ;; need to stop when we've hit the end of the buffer...
     (while (and (not (eobp)) (re-search-forward "^" nil t))
       (when (looking-at ".")
         (insert starting-dir)
         (forward-char 1)))
     (goto-char (point-min))
     (set (make-local-variable '*krb-output-base-directory*) starting-dir)
     (set (make-local-variable '*krb-output-base-file*) (buffer-file-name))
     (grep-mode))))

(defun krb-path-strip (path)
  "
    (krb-path-strip buffer-file-name)                  => \"/Users/\"
    (krb-path-strip (krb-path-strip buffer-file-name)) => \"/\"
    (krb-path-strip \"/Users/\")                       => \"/\"
    (krb-path-strip \"/Users\")                        => \"/\"
    (krb-path-strip \"/\")                             => \"/\"
    (krb-path-strip nil)                               => nil

"
  (cond ((null path)
         nil)
        (t
         (file-name-directory (directory-file-name (file-name-directory path))))))

(defun krb-find-containing-parent-directory-of-current-buffer (target-file-name &optional starting-directory)
  "Search backwards up the directory structure for the directory containing hte given literal file name).

 These examples are from a mac, using a (starting-directory of \"/Users/kburton/.emacs-local\")
   (krb-find-containing-parent-directory-of-current-buffer \".bashrc\")    => \"/Users/kburton/.bashrc\"
    (krb-find-containing-parent-directory-of-current-buffer \".localized\") => \"/Users/.localized\"
    (krb-find-containing-parent-directory-of-current-buffer \"Users\")      => \"/Users\"

"
  (let* ((path (or starting-directory (file-name-directory (buffer-file-name))))
         (candidate (format "%s%s" path target-file-name)))
    ;;     (message "krb-find-containing-parent-directory-of-current-buffer: target-file-name=%s path=%s candidate=%s"
    ;;              target-file-name path candidate)
    (cond
     ((file-exists-p candidate)
      path)
     ((or (null path)
          (string= "" path)
          (string= "/" path))
      (message "krb-find-containing-parent-directory-of-current-buffer: path is empty or '/', failing")
      nil)
     (t
      (krb-find-containing-parent-directory-of-current-buffer target-file-name (krb-path-strip path))))))

(defun krb-grep-thing-at-point-editable (git-cmd)
  (interactive (list (read-string "Search For: " (format "git grep --full-name -i -n '%s'" (or (symbol-at-point) "")))))
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
         (cmd (format "cd %s; %s" starting-dir git-cmd)))
    (krb-git-grep-do-grep starting-dir cmd)))

(global-set-key "\C-crg" 'krb-grep-thing-at-point-editable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execute a saved expression F6 / C-F6
(defvar krb-f6-expression "\"Use C-F6 to save an expression\"")
(make-variable-buffer-local 'krb-f6-expression)

(defun krb-f6 (arg)
  (interactive "P")
  '(cider-interactive-eval (cider-last-sexp))
  (if arg
      (setq krb-f6-expression (cider-last-sexp)))
  (cider-interactive-eval krb-f6-expression))

(defun krb-c-f6 ()
  (interactive)
  (setq krb-f6-expression (cider-last-sexp)))


'(

  (defun krbtmp ()
    (interactive)
    (cider-interactive-eval "(+ 2 3)"))

  (global-set-key (kbd "<f6>") 'krb-f6)
  )

(add-hook
 'clojure-mode-hook
 (lambda ()
   (paredit-mode +1)
   (setq abbrev-mode t)
   (local-set-key (kbd "<f6>")    'krb-f6)
   (local-set-key (kbd "C-<f6>")  'krb-c-f6)
   '(local-set-key (kbd "<f7>")   'krb-clojure-replay-inspect-expression)
   '(local-set-key (kbd "C-<f7>") 'krb-clojure-set-replay-inspect-expression)))

