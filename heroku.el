(require 'seq)

(defgroup heroku nil
  "Interact with Heroku CLI from Emacs"
  :prefix "heroku-")

(defvar heroku-app-list nil "List of Heroku apps fetched by heroku.el")

(defun heroku--list-service-string-p (s)
  "Check if a string is a service string"
  (or  (string= "" s) (cl-search "===" s) (cl-search "Process heroku" s)))

(defun heroku--not-list-service-string-p (s)
  "Check if a sting is NOT a service string"
  (not (heroku--list-service-string-p s)))

(defun heroku--parse-list (s)
  "Parse buffer output of heroku-list to produce a list of available apps"
  (let
      ((apps (seq-filter 'heroku--not-list-service-string-p (split-string s "\n"))))
    (mapcar 'car (mapcar 'split-string apps))
    ))

(defun heroku--parse-list-buffer-and-setq (buffer-name)
  (setq heroku-app-list
        (heroku--parse-list (with-current-buffer "*Heroku List*" (buffer-substring-no-properties (point-min) (point-max))))))

(defun heroku-list ()
  "List all Heroku apps and update heroku-app-list variable"
  (interactive)
  (message "Getting Heroku apps list...")
  (let ((buffer-name "*Heroku List*"))
    (if (get-buffer buffer-name)
        (kill-buffer buffer-name))
    (make-comint-in-buffer "heroku-list" buffer-name "heroku" nil "list")
    (switch-to-buffer-other-window buffer-name)
    (sleep-for 5)
    (heroku--parse-list-buffer-and-setq buffer-name)))

(defun heroku-bash ()
  "Connect to bash on a Heroku app"
  (interactive)
  (let* ((app-name (completing-read "Enter Heroku app name: " heroku-app-list)) (buffer-name (format "*Heroku Bash: %s" app-name)))
    (message (format "Connecting to bash on %s..." app-name))
    (make-comint-in-buffer "heroku-bash" buffer-name "heroku" nil "run" "bash" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)
    ))

(defun heroku-python ()
  "Connect to Python on a Heroku app"
  (interactive)
  (let* ((app-name (completing-read "Enter Heroku app name: " heroku-app-list)) (buffer-name (format "*Heroku Python: %s" app-name)))
    (message (format "Connecting to Python on %s..." app-name))
    (make-comint-in-buffer "heroku-python" buffer-name "heroku" nil "run" "python" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)
    ))

(defun heroku-logs ()
  "Connect and stream logs of a Heroku app"
  (interactive)
  (let* ((app-name (completing-read "Enter Heroku app name: " heroku-app-list)) (buffer-name (format "*Heroku Logs: %s" app-name)))
    (message (format "Gettings Heroku logs for %s..." app-name))
    (make-comint-in-buffer "heroku-logs" buffer-name "heroku" nil "logs" "-t" "-a" app-name)
    (sleep-for 3)
    (switch-to-buffer-other-window buffer-name)))

(defun heroku-destroy ()
  "Permanently destroy a Heroku app"
  (interactive)
  (let* ((app-name (completing-read "Enter Heroku app name: " heroku-app-list)) (buffer-name (format "*Heroku Destroy: %s" app-name)))
    (if  (and
          (yes-or-no-p (format "Do you really want destroy %s?" app-name))
          (string-equal app-name (read-string (format  "Type the app name once again to confirm [%s]: " app-name))))
        (progn
          (make-comint-in-buffer "heroku-destroy" buffer-name "heroku" nil "apps:destroy" "-a" app-name (format "--confirm=%s" app-name))
          (switch-to-buffer-other-window buffer-name))
      (message "Heroku command canceled"))))

(defun heroku-info ()
  "Show detailed Heroku app info"
  (interactive)
  (let* ((app-name (completing-read "Enter Heroku app name: " heroku-app-list)) (buffer-name (format "*Heroku Info: %s" app-name)))
    (message (format "Getting info on %s..." app-name))
    (make-comint-in-buffer "heroku-info" buffer-name "heroku" nil "apps:info" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)
    ))

(defun heroku-restart ()
  "Restart a Heroku app"
  (interactive)
  (let* ((app-name (completing-read "Enter Heroku app name: " heroku-app-list)) (buffer-name (format "*Heroku Restart Report: %s" app-name)))
    (message (format "Restarting heroku app:  %s..." app-name))
    (make-comint-in-buffer "heroku-restart" buffer-name "heroku" nil "restart" "-a" app-name)
    (message (ensure-buffer-string-blocking buffer-name))
    ))

(provide 'heroku)
