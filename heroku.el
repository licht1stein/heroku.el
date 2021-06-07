(defgroup heroku nil
  "Interact with Heroku CLI from Emacs"
  :prefix "heroku-")

(defun heroku-list ()
  "List all Heroku apps"
  (interactive)
  (message "Getting Heroku apps list...")
  (let ((buffer-name "*Heroku List*"))
    (make-comint-in-buffer "heroku-python" buffer-name "heroku" nil "list")
    (switch-to-buffer-other-window buffer-name)))

(defun heroku-bash ()
  "Connect to bash on a Heroku app"
  (interactive)
  (let* ((app-name (read-string "Enter Heroku app name: ")) (buffer-name (format "*Heroku Bash: %s" app-name)))
    (message (format "Connecting to bash on %s..." app-name))
    (make-comint-in-buffer "heroku-bash" buffer-name "heroku" nil "run" "bash" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)
    ))

(defun heroku-python ()
  "Connect to Python on a Heroku app"
  (interactive)
  (let* ((app-name (read-string "Enter Heroku app name: ")) (buffer-name (format "*Heroku Python: %s" app-name)))
    (message (format "Connecting to Python on %s..." app-name))
    (make-comint-in-buffer "heroku-python" buffer-name "heroku" nil "run" "python" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)
    ))

    (defun heroku-logs ()
      "Connect and stream logs of a Heroku app"
      (interactive)
      (let* ((app-name (read-string "Enter Heroku app name: ")) (buffer-name (format "*Heroku Logs: %s" app-name)))
        (message (format "Gettings Heroku logs for %s..." app-name))
        (make-comint-in-buffer "heroku-python" buffer-name "heroku" nil "logs" "-t" "-a" app-name)
        (sleep-for 3)
        (switch-to-buffer-other-window buffer-name)))

(defun heroku-restart ()
  "Restart a Heroku app"
  (interactive)
  (let* ((app-name (read-string "Enter Heroku app name: ")) (buffer-name (format "*Heroku Restart Report: %s" app-name)))
    (message (format "Restarting heroku app:  %s..." app-name))
    (make-comint-in-buffer "heroku-restart" buffer-name "heroku" nil "restart" "-a" app-name)
    (message (ensure-buffer-string-blocking buffer-name))
    ))

(provide 'heroku)
