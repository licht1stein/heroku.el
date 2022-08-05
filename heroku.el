;;; heroku.el --- Heroku CLI for Emacs  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2022 Mykhaylo Bilyanskyy <mb@blaster.ai>

;; Author: Mykhaylo Bilyanskyy
;; URL: https://github.com./licht1stein/heroku.el
;; Keywords: heroku, devops, convenience
;; Version: 2.0
;; Package-Requires: ((emacs "27.2") (transient "20220717.1713"))

;;; Commentary:
;; Magit inspired Heroku CLI client for Emacs.

;;; Code:
(require 'transient)
(require 'dash)
(require 's)

(defvar-local heroku-timestamp-regex "^[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}T[[:digit:]:\+\.]*")

(defcustom heroku-app-list nil
  "List of apps on Heroku."
  :group 'heroku
  :type 'list)

(comment
 (setq heroku-app-list nil))

(defun heroku-get-app-list ()
  "Run heroku apps and parse all apps into a list of strings."
  (->> (shell-command-to-string "heroku apps -A")
       (s-split "\n")
       (-filter (lambda (s) (and (not (s-starts-with-p "===" s)) (s-match heroku-app-name-re s))))
       (-filter (lambda (s) (not (string= "" s))))
       (-sort #'string<)
       (-map #'heroku--extract-app-details)))

(defun heroku-get-app-config (app)
  (interactive)
  (message (format "Getting app config for %s..." app))
  (->> (shell-command-to-string (format "heroku config -a %s" app))
       (s-split "\n")
       (-filter (lambda (s) (s-contains-p ":" s)))
       (-map (lambda (s) (s-split-up-to ":" s 1)))
       (-map (lambda (el) (list (s-trim (car el)) (s-trim-left (cadr el)))))))

(defun heroku-app-config-set (app key value)
  (message "Setting %s on %s..." key app)
  (let* ((result (->> (format "heroku config:set %s=%s -a %s" key value app)
		      call-process-shell-command)))
    (if (eq result 0)
	(message "%s" "Done.")
      (message "Something went wrong."))))

(defun heroku-app-config-unset (app key)
  (interactive (list heroku--app-name (car (heroku-get-config-kv))))
  (if (y-or-n-p (format "Are you sure you want to delete %s on %s?" key app))
      (progn
	(with-temp-message (format "Unsetting %s..." key)
	  (->> (format "heroku config:unset %s -a %s" key app)
	       call-process-shell-command))
	(heroku-app-config-refresh)
	(message "Done. App will restart."))))

(defun heroku-app-config-create (app key value)
  (interactive (list heroku--app-name (read-from-minibuffer "New key: ") (read-from-minibuffer "Value: ")))
  (if (alist-get key heroku--config-original nil nil 'string=)
      (progn
	(if (y-or-n-p (format "Key %s already exists. Edit it instead?" key))
	    (progn
	      (heroku-config-edit)
	      (erase-buffer)
	      (insert value))))
    (if (y-or-n-p (format "Create %s=%s on %s?" key value app))
	(progn (heroku-app-config-set app key value)
	       (heroku-app-config-refresh)))))

(defun heroku-refresh-app-list ()
  "Refresh list of app available to Heroku CLI."
  (interactive)
  (message "Refreshing Heroku app list...")
  (setq heroku-app-list (heroku-get-app-list))
  (message "Heroku app list refreshed"))

(defvar heroku-app-list-mode-map
  (let* ((map_ (make-sparse-keymap)))
    (define-key map_ (kbd "g") 'heroku-app-list-mode-refresh)
    (define-key map_ (kbd "l") 'heroku-logs-transient)
    (define-key map_ (kbd "r") 'heroku-run-transient)
    (define-key map_ (kbd "?") 'heroku-help-transient)
    (define-key map_ (kbd "c") 'heroku-app-config)
    map_)
  "Keymap for `heroku-app-list-mode'.")

(define-derived-mode heroku-app-list-mode tabulated-list-mode "Heroku Apps"
  "Heroku app list mode."
  (unless heroku-app-list
    (heroku-refresh-app-list))
  (let ((columns [("App" 50) ("Region" 20) ("Collab" 50)])
	(rows (->> heroku-app-list
		   (mapcar (lambda (x) `(nil [,@x]))))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode)))

(defvar heroku-app-config-mode-map
  (let* ((map_ (make-sparse-keymap)))
    (define-key map_ (kbd "?") 'heroku-config-transient)
    (define-key map_ (kbd "e") 'heroku-config-edit)
    (define-key map_ (kbd "g") 'heroku-app-config-refresh)
    (define-key map_ (kbd "d") 'heroku-app-config-unset)
    (define-key map_ (kbd "c") 'heroku-app-config-create)
    map_))

(define-derived-mode heroku-app-config-mode tabulated-list-mode "Heroku App Config"
  "Heroku app config and details mode."
  (let ((columns [("Variable" 50) ("Value" 50)])
	(rows (->> heroku--config-original
		   (mapcar (lambda (x) `(nil [,@x]))))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode)))

(defun heroku-app-config-refresh ()
  (interactive)
  (message "Refreshing %s config..." heroku--app-name)
  (setq heroku--config-original (heroku-get-app-config heroku--app-name))
  (heroku-app-config-mode)
  (message "Done. App will restart."))

(defun heroku-app-config (app)
  "Show environment variables for app."
  (interactive (list (heroku-get-app-name)))
  (let ((buff (format "*Heroku Config: %s" app)))
    (switch-to-buffer buff)
    (setq heroku--app-name app)
    (setq heroku--config-original (heroku-get-app-config app))
    (heroku-app-config-mode)))

(define-derived-mode heroku-logs-mode comint-mode "Heroku Logs"
  (read-only-mode))

(defvar-local heroku--logs-font-rules
    `((".*DEBUG.*" . 'shadow)
      (".*INFO.*" . 'term)
      (".*WARN.*" . 'warning)
      (".*ERROR.*" . 'transient-amaranth)
      (".*CRITICAL.*" . 'transient-red)
      (,heroku-timestamp-regex 0 (progn
				   (if heroku-logs-hide-timestamp-prefix
				       (add-text-properties (match-beginning 0)
							    (match-end 0)
							    '(invisible t)))))))

(font-lock-add-keywords 'heroku-logs-mode heroku--logs-font-rules)

(defun heroku-get-app-name ()
  (aref (tabulated-list-get-entry) 0))

(defun heroku-get-app-name-propertized ()
  (propertize (heroku-get-app-name) 'face 'transient-argument))

(defun heroku-get-logs (&optional args)
  (interactive (list (transient-args 'heroku-logs-transient)))
  (let* ((app (heroku-get-app-name))
	 (buffer (format "*Heroku Logs: %s*" app)))
    (message (format "Gettings Heroku logs for %s..." app))
    (apply #'make-comint-in-buffer "heroku-logs" buffer "heroku" nil "logs" "-a" app args)
    (with-current-buffer buffer
      (heroku-logs-mode)
      (pop-to-buffer-same-window buffer))))

(transient-define-prefix heroku-logs-transient ()
  :value (list "--tail")
  [[:description (lambda () (s-concat "Get logs for " (heroku-get-app-name-propertized)))

		 "\nArguments"
		 ("-d" "only show output from this dyno type (web, worker)" "--dyno=")
		 ("-n" "number of lines to display" "--num=")
		 ("-r" "git remote of app to use" "--remote=")
		 ("-s" "only show output from this source (app, heroku)" "--source=")
		 ("-t" "continually stream logs" "--tail")]]
  [["Execute"
    ("l" "display log output" heroku-get-logs)]])

(defun heroku-run-command (command &optional args detached)
  "Run a one-off process inside heroku dyno."
  (interactive (list (read-from-minibuffer "Command to run: ") (transient-args 'heroku-run-transient) nil))
  (let* ((app (heroku-get-app-name))
	 (buffer-name (format "*Heroku Run: %s" app)))
    (message (format "Running %s on %s..." command app))
    (if detached
	(async-shell-command (s-join " " `("heroku run:detached -a" ,app ,command ,@args)))
      (progn
	(apply #'make-comint-in-buffer "heroku-run" buffer-name "heroku" nil "run" command "-a" app args)
	(pop-to-buffer-same-window buffer-name)))))

(defun heroku-run-detached (command &optional args)
  (interactive (list (read-from-minibuffer "Command to run: ") (transient-args 'heroku-run-transient)))
  (heroku-run-command command args t))

(defun heroku-run-python (&optional args)
  (interactive (list (transient-args 'heroku-run-transient)))
  (heroku-run-command "python"))

(defun heroku-run-bash (&optional args)
  (interactive (list (transient-args 'heroku-run-transient)))
  (heroku-run-command "bash"))

(defun heroku-get-config-kv ()
  (list (aref (tabulated-list-get-entry) 0)
	(aref (tabulated-list-get-entry) 1)))

(defvar heroku-env-edit-mode-map
  (let* ((map_ (make-sparse-keymap)))
    (define-key map_ (kbd "C-c c") 'heroku-config-edit-save)
    (define-key map_ (kbd "C-c C-k") 'heroku-config-edit-cancel)
    map_)
  "Keymap for `heroku-env-edit-mode'.")

(defun heroku-config-edit-cancel ()
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(defun heroku-config-edit-save ()
  (interactive)
  (let ((new-value (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string= new-value heroku--env-old-value)
	(progn
	  (message "Value has not changed. If you want to abort press C-c C-k"))
      (if (y-or-n-p "Do you want to save changes to Heroku?")
	  (progn (message "Saving to Heroku...")
		 (heroku-app-config-set heroku--app-name heroku--env-key new-value)
		 (message (s-concat heroku--env-key " " new-value))
		 (heroku-config-edit-cancel)
		 (heroku-app-config-refresh))))))

(define-derived-mode heroku-env-edit-mode fundamental-mode "Heroku Edit Env"
  (defvar-local heroku--env-key nil)
  (defvar-local heroku--env-old-value nil)
  (message "Press `C-c c' to save or `C-c C-k` to cancel"))

(defun heroku-config-edit ()
  (interactive)
  (let* ((kv (heroku-get-config-kv))
	 (key (car kv))
	 (value (cadr kv))
	 (win (split-window-below))
	 (buff (format "*heroku-edit")))
    (message (format "Editing environment variable %s..." key))
    (select-window win)
    (switch-to-buffer (get-buffer-create buff))
    (heroku-env-edit-mode)
    (setq heroku--env-key key)
    (setq heroku--env-old-value value)
    (insert value)))

(transient-define-prefix heroku-config-transient ()
  [[:description (lambda () (s-concat "Config for " heroku--app-name))
		 ""]]
  [["Commands"
    ("g" "Refresh" heroku-app-config-refresh)
    ("u" "Unset" heroku-app-config-unset)
    ("e" "Edit" heroku-config-edit)]])

(transient-define-prefix heroku-run-transient ()
  [[:description (lambda () (s-concat "Run a one-off process inside " (heroku-get-app-name-propertized)))
		 ""]]

  [["Arguments"
    ("-e" "environment variables to set (use ';' to split multiple vars)" "env=")
    ("-r" "git remote of app to use" "--remote=")
    ("-s" "dyno size" "--size=")
    ("-x" "passthrough the exit code of the remote command" "--exit-code")
    ("-nn" "disables notification when dyno is up (alternatively use HEROKU_NOTIFICATIONS=0)" "--no-notify")
    ("-nt" "force the command to not run in a tty" "--no-tty")
    ("-t" "process type" "--type=")]]

  [["Run (prompt)"
    ("r" "run" heroku-run-command)
    ("d" "run:detached" heroku-run-detached)]
   ["Run command"
    ("b" "bash" heroku-run-bash)
    ("p" "python" heroku-run-python)]])

(transient-define-prefix heroku-help-transient ()
  [[:description "Heroku.el commands"]]
  [["Commands"
    ("g" "Refresh" heroku-app-list-mode-refresh)
    ("c" "Config" heroku-app-config)
    ("l" "Logs" heroku-logs-transient)
    ("r" "Run" heroku-run-transient)]]
  [["Heroku.el"
    ("?" "Help" heroku-help-transient)
    ("q" "Quit" quit-window)]])

(defvar-local heroku-app-name-re "^[[:alnum:]-]*")
(defvar-local heroku-region-re "\(\\([[:alnum:]]*\\)\)")
(defvar-local heroku-collab-re "[[:alnum:]]*@[[:alnum:]\.-_]*")

(defun heroku--extract-app-details (s)
  (let ((name (s-match heroku-app-name-re s))
	(region (cdr (s-match heroku-region-re s)))
	(collab (s-match heroku-collab-re s)))
    (-flatten (list name (or region "us") (or collab "private")))))

(defun heroku-app-list-mode-refresh ()
  (interactive)
  (heroku-refresh-app-list)
  (heroku-app-list-mode))



;;;###autoload
(defun heroku-list ()
  "Start heroku.el and choose app to operate on."
  (interactive)
  (let ((buff "*Heroku Apps*"))
    (switch-to-buffer buff)
    (heroku-app-list-mode)))

(provide 'heroku)
;;; heroku.el ends here






(comment
 (defvar-local heroku-view-title nil)

 (defvar heroku-target-process nil "Process to run in heroku run.")
 (setq heroku-target-email nil)

 (comment
  (setq heroku-target-process nil))


 (defvar-local heroku-app-name-regex "[[:graph:]]*\\[[[:alnum:]\.]*\]")


 (defun heroku-cli-installed-p ()
   "Check if heroku cli is installed."
   (interactive)
   (let* ((output (shell-command-to-string "heroku")))
     (s-starts-with-p "CLI to interact with Heroku" output)))

 (comment
  (heroku-cli-installed-p))

 (defface heroku-selected-app-face
   '((((class color) (background light)) :foreground "DarkOliveGreen4")
     (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
   "Face for selected app."
   :group 'heroku-faces)

 (defface heroku-warning-face
   '((t :inherit warning))
   "Face for warnings."
   :group 'heroku-faces)

 (defcustom heroku-logs-hide-timestamp-prefix nil
   "Hide heroku timestamp prefix in logs.

Useful if your logging system prints it's own timestamp."
   :group 'heroku
   :type 'boolean)



 (defun heroku-select-app-fn (&rest args)
   (interactive)
   (message (pp-to-string args))
   (->> (completing-read "Select app: " heroku-app-list)
	(setq heroku-selected-app)))

 (defclass heroku-view--variable (transient-variable)
   ;; FIXME: We don't need :scope, but maybe a slot has to be defined.
   ((scope       :initarg :scope)))

 (defun heroku--header ()
   (s-concat (propertize  "Selected app: " 'face 'transient-heading)
	     (if heroku-selected-app
		 (propertize heroku-selected-app 'face 'heroku-selected-app-face)
	       (propertize  "No app selected, please select" 'face 'heroku-warning-face))
	     "\n"))

 (defun heroku--header-w-target-user ()
   (s-concat (heroku--header)
	     (propertize "Target email: " 'face 'transient-heading)
	     (if heroku-target-email
		 (propertize heroku-target-email 'face 'heroku-selected-app-face)
	       (propertize "No target user set, please set" 'face 'heroku-warning-face))
	     "\n"))

 (defun heroku--header-w-target-command ()
   (s-concat (heroku--header)
	     (propertize "Command: " 'face 'transient-heading)
	     (if heroku-target-process
		 (propertize heroku-target-process 'face 'heroku-selected-app-face)
	       (propertize "No command set, please set" 'face 'heroku-warning-face))
	     "\n"))


 (defun heroku-make-invisible (my-re)
   (save-excursion
     (goto-char (point-min))
     (while (re-search-forward my-re nil t)
       (let* ((invisible-overlay (make-overlay (match-beginning 0) (match-end 0))))
	 (overlay-put invisible-overlay 'invisible t)))))








 (defun heroku-logs-toggle-timestamps ()
   "Toggle `heroku-logs-hide-timestamp-prefix'."
   (interactive)
   (setq heroku-logs-hide-timestamp-prefix (not heroku-logs-hide-timestamp-prefix)))

 (defun heroku-logs-toggle-timestamps ()
   "Toggle `heroku-logs-hide-timestamp-prefix'."
   (interactive)
   (setq heroku-logs-hide-timestamp-prefix (not heroku-logs-hide-timestamp-prefix))
   (heroku-refresh-font-lock))



 (defun heroku-app-promote ()
   "Run pipelines:promote on app."
   (interactive)
   (unless heroku-selected-app
     (heroku-select-app "Select app to promote: "))
   (if (y-or-n-p (format "Do you want to promote %s?" heroku-selected-app))
       (progn
	 (message (format "Promoting app %s..." heroku-selected-app))
	 (async-shell-command (format "heroku pipelines:promote -a %s" heroku-selected-app)))))

 (defun heroku-run-command-general (command &optional args)
   "Run a one-off process inside heroku dyno."
   (interactive (list (transient-args 'heroku-run-dispatch)))
   (unless heroku-selected-app (heroku-select-app-fn "Select app to run command: "))
   (unless heroku-target-process (setq heroku-target-process (read-from-minibuffer "Command to run: ")))
   (message (format "Running %s on %s..." heroku-target-process heroku-selected-app))
   (let* ((buffer-name (format "*Heroku Run: %s" heroku-selected-app)))
     (apply #'make-comint-in-buffer `("heroku-run" ,buffer-name "heroku" nil ,command ,heroku-target-process "-a" ,heroku-selected-app ,@args))
     (pop-to-buffer-same-window buffer-name)))

 (comment
  (make-comint-in-buffer "heroku-run" "*heroku run test" "heroku" nil "run" "python" "-a" "closerbot")
  (heroku-run-command))

 (defun heroku-run-detached (&optional args)
   (interactive (list (transient-args 'heroku-run-dispatch)))
   (async-shell-command  (apply ) `("heroku-run" ,buffer-name "heroku" nil ,command ,heroku-target-process "-a" ,heroku-selected-app ,@args)))

 (defun heroku-run-command (&optional args)
   (interactive)
   (heroku-run-command-general "run" args))

 (defun heroku-run-python (&optional args)
   (interactive)
   (setq heroku-target-process "python")
   (heroku-run-command args))

 (defun heroku-run-bash (&optional args)
   (interactive)
   (setq heroku-target-process "bash")
   (heroku-run-command args))

 (defmacro heroku-access-defcommand (name command prefix)
   `(defun ,name (&optional args)
      (interactive (list (transient-args ,prefix)))
      (message (s-concat "args:" (pp-to-string args)))
      (cond
       ((not heroku-selected-app) (message "Error: No Heroku app selected"))
       ((not heroku-target-email) (message "Error: No target email selected"))
       (t (->> (list "heroku" ,command heroku-target-email args "-a" heroku-selected-app)
	       -flatten
	       (s-join " ")
	       async-shell-command)))))

 (comment
  (defmacro heroku-simple-defcommand (command prefix)
    (let* ((s-command (pp-to-string command))
	   (func-name (->> (s-concat "heroku-" s-command) make-symbol))
	   (docstring (s-concat "Execute Heroku command " s-command ".")))
      `(defun ,func-name (&optional args)
	 ,docstring
	 (interactive (list (transient-args ,prefix)))
	 (->> (list "heroku" ,s-command args (if heroku-selected-app (s-concat "-a" heroku-selected-app)))
	      -flatten
	      (s-join " ")
	      async-shell-command)))))


 (heroku-access-defcommand heroku-access-add "access:add" 'heroku-access-dispatch)
 (heroku-access-defcommand heroku-access-remove "access:remove" 'heroku-access-dispatch)
 (heroku-access-defcommand heroku-access-update "access:update" 'heroku-access-dispatch)

 (comment
  (declare heroku-addons-dispatch)


  (heroku-simple-defcommand addons 'heroku-addons-dispatch)
  (heroku-simple-defcommand addons:attach 'heroku-addons-dispatch)
  (heroku-simple-defcommand addons:create 'heroku-addons-dispatch)
  (heroku-simple-defcommand addons:destroy 'heroku-addons-dispatch)
  (heroku-simple-defcommand addons:detach 'heroku-addons-dispatch))

 (defun heroku/make-command-handler (prefix command)
   (lambda (&optional args)
     (interactive (list (transient-args 'prefix)))
     (message (pp-to-string args))))


 (defun see-args (prompt init-inp history)
   (message (pp-to-string prompt))
   (message (pp-to-string init-inp))
   (message (pp-to-string history)))

 (defvar heroku-selected-app nil)
 (transient-define-infix heroku-option:--app ()
   :description "Select Heroku app to run commands against!"
   :class 'transient-option
   :shortarg "a"
   :argument "--app="
   :reader (lambda (a1 a2 a3)
	     (let* ((app (completing-read "Select app: " heroku-app-list)))
	       (setq heroku-selected-app app)
	       app)))

 (transient-define-prefix heroku-dispatch ()
   :value (list (format "--app=%s" heroku-selected-app))
   [[:description "Heroku dispatcher\n"

		  "\nApp"
		  (heroku-option:--app)]
    ]
   [["Commands"
     ;; ("a" "Access" heroku-access-dispatch)
     ;; ("A" "Addons" heroku-addons-dispatch)
     ("l" "Logs" heroku-logs-dispatch)
     ("r" "Run" heroku-run-dispatch)
     ("p" "Promote" heroku-app-promote)

     ;; ("f" "try-maker" (heroku/make-command-handler 'heroku-addons-dispatch "command:foo"))
     ]])

 (transient-define-prefix heroku-access-dispatch ()
   [[:description heroku--header-w-target-user
		  "Required"
		  ("s" "Select target app" heroku-select-app)
		  ("e" "Set target email" heroku-set-target-user)]]
   [["Arguments"
     ("-r" "git remote of app to use" "--remote=")
     ("-p" "list of permissions comma separated (deploy, manage, operate)" "--permissions=")
     ("-j" "output in json format" "--json")]]
   [["Execute"
     ("a" "access:add     add new user to your app" heroku-access-add)
     ("r" "access:remove  remove users from a team app" heroku-access-remove)
     ("a" "access:update  update existing collaborators on an team app" heroku-access-update)]])



 (transient-define-prefix heroku-pipelines-dispatch ()
   [[:description heroku--header
		  "Work with pipelines you have access to\n"

		  [["Arguments"
		    ("-j" "output in json format" "--json")]]
		  [[
		    "Commands"
		    ("f" "foo" "foo")]]]])

 (transient-define-prefix heroku-run-dispatch ()
   [[:description heroku--header-w-target-command
		  "run a one-off process inside a heroku dyno\n"
		  "Required"
		  ("s" "Select target app" heroku-select-app)
		  ("c" "Command to run (e.g. bash)" heroku-select-run-command)

		  "\nArguments"
		  ("-e" "environment variables to set (use ';' to split multiple vars)" "env=")
		  ("-r" "git remote of app to use" "--remote=")
		  ("-s" "dyno size" "--size=")
		  ("-x" "passthrough the exit code of the remote command" "--exit-code")
		  ("-nn" "disables notification when dyno is up (alternatively use HEROKU_NOTIFICATIONS=0)" "--no-notify")
		  ("-nt" "force the command to not run in a tty" "--no-tty")
		  ("-t" "process type" "--type=")]]
   [["Execute"
     ("r" "run" heroku-run-command)
     ("d" "run:detached" heroku-run-detached)
     ("B" "run bash" heroku-run-bash)
     ("p" "run python" heroku-run-python)]])

 (comment
  (transient-define-prefix heroku-addons-dispatch ()
    "Dispatch heroku run."
    [[:description heroku--header
		   "App"
		   ("s" "Select target app" heroku-select-app)
		   ("S" "Clear selected app" heroku-clear-app)

		   "\nArguments"
		   ("-r" " git remote of app to use" "--remote=")
		   ("-j" " output in json format" "--json")
		   ("-as" "--as=as                  name for add-on attachment" "--as=")
		   ("-c"  " --confirm=confirm        overwrite existing add-on attachment with same name" "--confirm=")
		   ("-C" " --credential=credential  credential name for scoped access to Heroku Postgres" "--credential=")]]
    [["Execute"
      ("l" " addons            list your add-ons and attachments" heroku-addons)
      ("a" " addons:attach     attach an existing add-on resource to an app" heroku-addons:attach)
      ("a" " addons:create     attach an existing add-on resource to an app" heroku-addons:create)
      ("d" " addons:destroy    permanently destroy an add-on resource" heroku-addons:destroy)
      ("de" " addons:detach     detach an existing add-on resource from an app" heroku-addons:detach)]]))


 (transient-define-infix heroku-set-target-user ()
   :description "Set target user email"
   :class 'heroku-view--variable
   :variable 'heroku-target-email
   :argument ""
   :reader (lambda (prompt _initial-input history)
	     (setq heroku-target-email (read-from-minibuffer "Email: "))))



 (transient-define-infix heroku-select-run-command ()
   :description "Set command to run."
   :class 'heroku-view--variable
   :variable 'heroku-target-process
   :argument ""
   :reader (lambda (prompt _initial-input history)
	     (setq heroku-target-process (completing-read "Command: " heroku-target-process))))


 (define-infix-command heroku-foo ()
   ;; TODO: Add an asterisk or something when the view has been modified but not saved.
   :description (lambda () (message "foo"))
   :class 'heroku-view--variable
   :argument ""
   :variable 'heroku-selected-app
   :prompt "Title: "
   :reader (lambda (prompt _initial-input history)
             ;; FIXME: Figure out how to integrate initial-input.
             (read-string prompt (when heroku-view-title
                                   (format "%s" heroku-view-title))
                          history))))
