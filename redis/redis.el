(require 'comint)

(defvar redis-cli-executable "redis-cli")
(setq redis-cli-executable "redis/redis-2.4.11/src/redis-cli")

(defvar inferior-redis-mode-map nil)
(unless inferior-redis-mode-map
  (setq inferior-redis-mode-map
        (copy-keymap comint-mode-map)))

(put 'inferior-redis-mode 'mode-class 'special)

(defun inferior-redis-mode ()
  (interactive)
  (comint-mode)
  ;; (setq comint-prompt-regex "redis [:digit:]\\{3\\}.[:digit:]\\{3\\}.[:digit:]\\{3\\}.[:digit:]\\{3\\}:[:digit:]+> ")
  (setq comint-prompt-regex "redis ")
  (setq major-mode 'inferior-redis-mode)
  (seq mode-name "Inferior Redis")
  (setq mode-line-process '(":%s"))
  (use-local-map 'inferior-redis-mode-map))


(defun inferior-redis ()
  (interactive)
  (message redis-cli-executable)
  (if (not (comint-check-proc "*inferior-redis*"))
      (progn
        (set-buffer (apply (function make-comint)
                           "inferior-redis"
                           ;; 3rd arg is argv for executable
                           redis-cli-executable nil '())))) (pop-to-buffer "*inferior-redis*"))

(defalias 'run-redis 'inferior-redis)

(provide 'inferior-redis-mode)