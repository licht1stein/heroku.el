;;; heroku.el --- Emacs client for Heroku

;;; Commentary:
;;  A simple front-end for Heroku CLI.

(require 'seq)

;;; Code:

(defgroup heroku nil
  "Interact with Heroku CLI from Emacs"
  :prefix "heroku-")

(defvar heroku-app-list nil "List of Heroku apps fetched by heroku.el.")

(defun heroku--list-service-string-p (s)
  "Check if a string is a service string.
Argument S Checks if s should be excluded from the app-list."
  (or  (string= "" s) (cl-search "===" s) (cl-search "Process heroku" s)))

(defun heroku--not-list-service-string-p (s)
  "Check if a sting is NOT a service string.
Argument S Checks if s is an app name."
  (not (heroku--list-service-string-p s)))

(defun heroku--parse-list (s)
  "Parse buffer output of `heroku-list' to produce a list of available apps.
Argument S string to be parsed into a list of app names."
  (let
      ((apps (seq-filter 'heroku--not-list-service-string-p (split-string s "\n"))))
    (mapcar 'car (mapcar 'split-string apps))))

(defun heroku--parse-list-buffer-and-setq (buffer-name)
  "Parse `BUFFER-NAME` and convert it's contents into a list of app names."
  (setq heroku-app-list
        (heroku--parse-list (with-current-buffer "*Heroku List*" (buffer-substring-no-properties (point-min) (point-max))))))

(defun heroku--silently-update-app-list ()
  "Silently fetch all Heroku apps and update `heroku-app-list' variable."
  (message "Getting Heroku apps list...")
  (let ((buffer-name "*Heroku List*"))
    (if (get-buffer buffer-name)
        (kill-buffer buffer-name))
    (make-comint-in-buffer "heroku-list" buffer-name "heroku" nil "list")
    (sleep-for 5)
    (heroku--parse-list-buffer-and-setq buffer-name)
    (kill-buffer buffer-name)))

(defun heroku--read-app-name ()
  "Prompt user to enter or select an app.
If `heroku-app-list' is nil, also prompt if user wants to have the app-list updated."
  (unless heroku-app-list
    (if (y-or-n-p "App list is empty, do you want to update? ")
        (heroku--silently-update-app-list)))
  (completing-read "Enter Heroku app name: " heroku-app-list))

(defun heroku-list ()
  "List all Heroku apps and update `heroku-app-list' variable."
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
  "Connect to bash on a Heroku app."
  (interactive)
  (let* ((app-name (heroku--read-app-name)) (buffer-name (format "*Heroku Bash: %s" app-name)))
    (message (format "Connecting to bash on %s..." app-name))
    (make-comint-in-buffer "heroku-bash" buffer-name "heroku" nil "run" "bash" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)
    ))

(defun heroku-python ()
  "Connect to Python on a Heroku app."
  (interactive)
  (let* ((app-name (heroku--read-app-name)) (buffer-name (format "*Heroku Python: %s" app-name)))
    (message (format "Connecting to Python on %s..." app-name))
    (make-comint-in-buffer "heroku-python" buffer-name "heroku" nil "run" "python" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)
    ))

(defun heroku-destroy ()
  "Permanently destroy a Heroku app."
  (interactive)
  (let* ((app-name (heroku--read-app-name)) (buffer-name (format "*Heroku Destroy: %s" app-name)))
    (if  (and
          (yes-or-no-p (format "Do you really want destroy %s?" app-name))
          (string-equal app-name (read-string (format  "Type the app name once again to confirm [%s]: " app-name))))
        (progn
          (make-comint-in-buffer "heroku-destroy" buffer-name "heroku" nil "apps:destroy" "-a" app-name (format "--confirm=%s" app-name))
          (switch-to-buffer-other-window buffer-name))
      (message "Heroku command canceled"))))

(defun heroku-buildpacks-list ()
  "Display the buildpacks for a Heroku app."
  (interactive)
  (let* ((app-name (heroku--read-app-name)) (buffer-name (format "*Heroku Buildpacks List: %s" app-name)))
    (message (format "Getting buildpacks for %s..." app-name))
    (make-comint-in-buffer "heroku-buildpacks-list" buffer-name "heroku" nil "buildpacks" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)
    ))

(defun heroku-info ()
  "Show detailed Heroku app info."
  (interactive)
  (let* ((app-name (heroku--read-app-name)) (buffer-name (format "*Heroku Info: %s" app-name)))
    (message (format "Getting info on %s..." app-name))
    (make-comint-in-buffer "heroku-info" buffer-name "heroku" nil "apps:info" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)
    ))

(defun heroku-rename ()
  "Rename a Heroku app."
  (interactive)
  (let* ((app-name (heroku--read-app-name)) (new-name (read-string (format "Enter new name for %s: " app-name))) (buffer-name (format "*Heroku Rename: %s" app-name)))
    (if (gnus-yes-or-no-p (format "Confirm renaming %s -> %s?" app-name new-name))
        (progn
          (make-comint-in-buffer "heroku-rename" buffer-name "heroku" nil "apps:rename" "-a" app-name new-name)
          (switch-to-buffer-other-window buffer-name))
      (message "Heroku command canceled"))))

(defun heroku-restart ()
  "Restart a Heroku app."
  (interactive)
  (let* ((app-name (heroku--read-app-name)) (buffer-name (format "*Heroku Restart Report: %s" app-name)))
    (message (format "Restarting heroku app:  %s..." app-name))
    (make-comint-in-buffer "heroku-restart" buffer-name "heroku" nil "restart" "-a" app-name)
    (message (ensure-buffer-string-blocking buffer-name))))

(defun heroku-addons ()
  "Show add-ons info for a Heroku app."
  (interactive)
  (let* ((app-name (heroku--read-app-name)) (buffer-name (format "*Heroku Addons: %s" app-name)))
    (message (format "Getting info on %s add-ons..." app-name))
    (make-comint-in-buffer "heroku-addons-info" buffer-name "heroku" nil "addons" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)))

(defun heroku-config-set ()
  "Set an environment variable on a Heroku app."
  (interactive)
  (let* ((app-name (heroku--read-app-name))
         (buffer-name (format "*Heroku Config Set: %s" app-name))
         (env-name (read-string (format "Enter variable name on %s: " app-name)))
         (env-value (read-string (format "Enter variable value for %s: " env-name))))
    (if (gnus-yes-or-no-p (format "Confirm setting %s=%s on %s?" env-name env-value app-name))
        (progn (make-comint-in-buffer "heroku-config-set" buffer-name "heroku" nil "config:set"
                                      (format "%s=%s" env-name env-value) "-a" app-name)
               (message "Done: %s=%s on %s" env-name env-value app-name)))))

(defun heroku-config-list ()
  "List all environment variables on a Heroku app."
  (interactive)
  (let* ((app-name (heroku--read-app-name))
         (buffer-name (format "*Heroku Config: %s" app-name)))
    (message (format "Getting config variables for %s" app-name))
    (make-comint-in-buffer "heroku-config" buffer-name "heroku" nil "config" "-a" app-name)
    (switch-to-buffer-other-window buffer-name)))


(defvar heroku-logs-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-heroku-logs'")

(defun heroku-logs--initialize ()
  "Helper function to initialize Heroku-Logs"
  ;; (setq comint-process-echoes t)
  ;; (setq comint-use-prompt-regexp t)
  )

(defconst heroku-logs-keywords
  '("INFO" "DEBUG" "ERROR" "WARNING" "WARN"))

(defvar heroku-logs-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt heroku-logs-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `heroku-logs-mode'.")

(define-derived-mode heroku-logs-mode comint-mode "Heroku-Logs"
  "Major mode for `heroku-logs'.

\\<heroku-logs-mode-map>"
  nil "Heroku-Logs"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  ;; (setq comint-prompt-regexp heroku-logs-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(heroku-logs-font-lock-keywords t))
  ;; (set (make-local-variable 'paragraph-start) heroku-logs-prompt-regexp)
  )

(set (make-local-variable 'font-lock-defaults) '(heroku-logs-font-lock-keywords t))

;; this has to be done in a hook. grumble grumble.
(add-hook 'heroku-logs-mode-hook 'heroku-logs--initialize)

(defun heroku-logs ()
  "Connect and stream logs of a Heroku app."
  (interactive)
  (let* ((app-name (heroku--read-app-name)) (buffer-name (format "*Heroku Logs: %s" app-name)))
    (message (format "Gettings Heroku logs for %s..." app-name))
    (make-comint-in-buffer "heroku-logs" buffer-name "heroku" nil "logs" "-t" "-a" app-name)
    (pop-to-buffer-same-window buffer-name)
    (heroku-logs-mode)))

(provide 'heroku)

;;; heroku.el ends here
