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

(define-derived-mode heroku-app-list-mode tabulated-list-mode "Heroku"
  "Heroku app list mode."
  (let ((columns [("App" 50) ("Region" 20) ("Collab" 50)])
	(rows (->> heroku-app-list
		   (mapcar (lambda (x) `(nil [,x "us" "private"]))))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode)))

(define-derived-mode heroku-logs-mode comint-mode "Heroku Logs"
  (read-only-mode))

(defface heroku-grey-face
  '((((class grayscale) (background light)) :foreground "DimGray" :slant italic)
    (((class grayscale) (background dark))  :foreground "LightGray" :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "VioletRed4")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 16) (background light)) :foreground "RosyBrown")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 8)) :foreground "green")
    (t :slant italic))
  "Font Lock mode face used to highlight strings."
  :group 'heroku-faces)

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
  [[:description (lambda () "" (s-concat "Get logs for " (propertize (heroku-get-app-name) 'face 'transient-argument)))

		 "\nArguments"
		 ("-d" "only show output from this dyno type (web, worker)" "--dyno=")
		 ("-n" "number of lines to display" "--num=")
		 ("-r" "git remote of app to use" "--remote=")
		 ("-s" "only show output from this source (app, heroku)" "--source=")
		 ("-t" "continually stream logs" "--tail")]]
  [["Execute"
    ("l" "display log output" heroku-get-logs)]])

(transient-define-prefix heroku-help-transient ()
  [[:description "Heroku.el commands"]]
  [["Commands"
    ("g" "Refresh" heroku-app-list-mode-refresh)
    ("l" "Logs" heroku-logs-transient)]]
  [["Heroku.el"
    ("?" "Help" heroku-help-transient)
    ("q" "Quit" quit-window)]])

(defun heroku-get-app-list ()
  "Run heroku apps and parse all apps into a list of strings."
  (->> (shell-command-to-string "heroku apps -A")
       (s-match-strings-all "^[[:alnum:]-]*")
       -flatten
       (-filter (lambda (el) (not (string= "" el))))))

(defun heroku-refresh-app-list ()
  "Refresh list of app available to Heroku CLI."
  (interactive)
  (message "Refreshing Heroku app list...")
  (setq heroku-app-list (heroku-get-app-list))
  (message "Heroku app list refreshed"))

(defun heroku-app-list-mode-refresh ()
  (interactive)
  (heroku-refresh-app-list)
  (heroku-app-list-mode))

(defvar heroku-app-list-mode-map
  (let* ((map_ (make-sparse-keymap)))
    (define-key map_ (kbd "g") 'heroku-app-list-mode-refresh)
    (define-key map_ (kbd "l") 'heroku-logs-transient)
    (define-key map_ (kbd "?") 'heroku-help-transient)
    map_))

(defun heroku ()
  "Start heroku.el and choose app to operate on."
  (interactive)
  (let ((buff "*Heroku Apps*"))
    (switch-to-buffer buff)
    (heroku-app-list-mode)))





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


(provide 'heroku)
;;; heroku.el ends here
