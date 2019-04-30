(defvar krb-sql-history-ring '())

(make-local-variable 'krb-sql-history-ring)

(defun krb-sql-clear-history ()
  (interactive)
  (setq krb-sql-history-ring '()))

(defun krb-sql-send-region (idx)
  (interactive "P")
  (cond
   (idx
    (message "krb-sql-send-region: sending from history idx=%s" idx (nth idx krb-sql-history-ring))
    (message "krb-sql-send-region: sending from history %s/%s" idx (nth idx krb-sql-history-ring))
    (sql-send-string (nth idx krb-sql-history-ring)))
   (t
    (message "krb-sql-send-region: sending region %s" (buffer-substring (region-beginning) (region-end)))
    ;; TODO: only push (or send) if it's a non-empty string
    (setq krb-sql-history-ring
	  (cons (buffer-substring-no-properties (region-beginning) (region-end))
		krb-sql-history-ring))
    (sql-send-region (region-beginning) (region-end)))))

(defun krb-sql-history-show ()
  (interactive)
  (message "sql-history: %s" krb-sql-history-ring))

(defun krb-sql-mode-hook ()
  (interactive)
  (local-set-key (kbd "\C-crr") 'krb-sql-send-region))
