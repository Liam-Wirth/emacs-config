;;; my-custom-functions.el -*- lexical-binding: t; -*-

(defun my/safe-get-previous-daily-note ()
  "Safely get the previous daily note, even if not in a daily note."
  (let* ((today (format-time-string "%y-%m-%d"))
         (today-file (expand-file-name (concat today ".org") "~/org/roam/daily"))
         (current-file (buffer-file-name (current-buffer)))
         (daily-note (if (and current-file (string= (expand-file-name current-file) today-file))
                         current-file
                       today-file))
         (date (file-name-base daily-note)))
    (let ((prev-date (format-time-string "%y-%m-%d"
                                         (time-subtract (date-to-time date) (days-to-time 1))))
          (found-file nil))
      (while (and (not found-file) (string< prev-date date))
        (let ((prev-file (expand-file-name (concat prev-date ".org") "~/org/roam/daily")))
          (if (file-exists-p prev-file)
              (setq found-file prev-file)
            (setq prev-date (format-time-string "%y-%m-%d"
                                                (time-subtract (date-to-time prev-date) (days-to-time 1)))))))
      found-file)))

(defun transfer-todos-to-new-daily ()
  "Transfer unfinished TODO entries from the last daily note to the new one."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (today-file (org-roam-dailies--file-path today))
         (last-daily (safe-get-previous-daily-note))
         (last-daily-file (when last-daily (org-roam-node-file last-daily))))
    (when (and last-daily-file (file-exists-p last-daily-file))
      (with-current-buffer (find-file-noselect today-file)
        (goto-char (point-max))
        (insert "\n* Transferred TODOs\n")
        (insert-file-contents last-daily-file)
        (while (re-search-forward "^\\*+ \\(TODO\\|STARTED\\) " nil t)
          (beginning-of-line)
          (kill-line)
          (yank))
        (save-buffer))
      (with-current-buffer (find-file-noselect last-daily-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ \\(TODO\\|STARTED\\) " nil t)
          (org-todo 'done))
        (save-buffer)))))
