;;; heroku.el --- Heroku CLI similar to Magit  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2022 Mykhaylo Bilyanskyy <mb@blaster.ai>

;; Author: Mykhaylo Bilyanskyy
;; URL: https://github.com./licht1stein/heroku.el
;; Keywords: heroku, devops, convenience
;; Version: 2.0
;; Package-Requires: ((emacs "27.2") (transient "0.3.7") (dash "2.19.1") (s "1.12.0") (ts "0.2.2"))

;;; Commentary:
;; Magit inspired Heroku CLI client for Emacs.

;;; Code:
(require 'transient)
(require 'dash)
(require 's)
(require 'json)
(require 'ts)

(defvar-local heroku-timestamp-regex "^[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}T[[:digit:]:\+\.]*" "Regex pattern of heroku logs standard timestamp.")
(defvar-local heroku-app-name-re "^[[:alnum:]-]*" "Heroku app name regex.")
(defvar-local heroku-region-re "\(\\([[:alnum:]]*\\)\)" "Heroku region regex.")
(defvar-local heroku-collab-re "[[:alnum:]]*@[[:alnum:]\.-_]*" "Heroku collaborator regex.")

(defun heroku-some-string-p (s)
  "Return S if it's some not empty string, else nil."
  (cond
   ((eq s nil) nil)
   ((not (eq 'string (type-of s))) nil)
   ((string= s "") nil)
   (t s)))

(defcustom heroku-app-list nil
  "List of apps on Heroku."
  :group 'heroku
  :type 'list)

(defun heroku-get-app-details (app)
  "Run heroku app:details for APP and parse results."
  (interactive (list (heroku-get-app-name)))
  (with-temp-message (format "Getting details for %s..." app)
    (->>   (shell-command-to-string (format "heroku apps:info -a %s" app))
	   (s-split "\n")
	   (-filter (lambda (s) (s-contains-p ":" s)))
	   (-map (lambda (s) (s-split-up-to ":" s 1)))
	   (-map (lambda (el) (list (s-trim-left (car el)) (s-trim-left (cadr el))))))))

(defun heroku--get-in (path js &optional default)
  "Extract value from hashmpa JS under PATH.

Similar to Clojure's get-in."
  (or (cond ((eq :null js) nil)
	    ((eq (length path) 1) (gethash (car path) js))
	    (t (heroku--get-in (cdr path) (gethash (car path) js))))
      default))

(comment
 (setq heroku-app-list nil)
 (heroku--get-in '("team" "name") (car tapps))
 (heroku--get-in '("owner" "email") (car tapps)))

(defvar-local heroku--red-cross (propertize "⨯" 'face 'transient-red))
(defvar-local heroku--green-check (propertize "✓" 'face 'transient-value))

(defun heroku--app-list-data (js)
  `(("Name" 25 ,(propertize (gethash "name" js) 'face 'bold))
    ("Region" 5 ,(heroku--get-in '("region" "name") js "s"))
    ("Owner" 25 ,(heroku--get-in '("owner" "email") js))
    ("Team" 10 ,(heroku--get-in '("team" "name") js "-"))
    ("Org" 10 ,(heroku--get-in '("organization" "name") js "-"))
    ("Maint" 5 ,(if (eq :false (heroku--get-in '("maintenance") js)) "" heroku--red-cross))
    ("Created" 12 ,(ts-format "%Y-%m-%d" (ts-parse (gethash "created_at" js))))
    ("Updated" 20 ,(ts-format "%Y-%m-%d %H:%M:%S" (ts-parse (gethash "updated_at" js))))
    ("Stack" 10 ,(heroku--get-in '("stack" "name") js))
    ("ACM" 5 ,(if (eq :false (gethash "acm" js)) heroku--red-cross heroku--green-check))))

(defun heroku-alist-get (key al)
  (alist-get key al nil nil 'string=))

(defun heroku-get-app-list ()
  (let* ((json-object-type 'hash-table)
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (raw (shell-command-to-string "heroku apps -A --json"))
	 (json (json-parse-string raw))
	 (data (-> json (append nil))))
    (-map #'heroku--app-list-data data)))

(comment
 (heroku-get-app-list-2)
 (if (eq :false (gethash "acm")) "⨯" "✓")
 (gethash "buildpack_provided_description" (car tapps))
 (gethash "archived_at" (car tapps))
 (setq tdata (heroku--app-list-data (car tapps)))
 (alist-get "name" tdata nil nil 'string=)
 (hash-table-keys (car tapps))
 ;; =>
 ;; ("web_url"
 ;; "updated_at"
 ;; "stack"
 ;; "slug_size"
 ;; "repo_size"
 ;; "released_at"
 ;; "internal_routing"
 ;; "space"
 ;; "team"
 ;; "organization"
 ;; "region"
 ;; "owner"
 ;; "name"
 ;; "maintenance"
 ;; "git_url"
 ;; "id"
 ;; "created_at"
 ;; "build_stack"
 ;; "buildpack_provided_description"
 ;; "archived_at"
 ;; "acm")
 (setq tapps (->> (heroku-get-app-list-2)))
 (setq intkeys '("name" "team" "owner" "build_stack" "released_at" "updated_at"))
 (setq tteam (gethash "team" (car tapps)))
 (heroku-apps-json-get-team (car tapps))
 (hash-table-keys tteam)

 (setq tkeys
       (-> tapps
	   car
	   hash-table-keys))
 (->> tapps
      car
      (gethash "web_url")))


(defun heroku-app-destroy (app)
  "Destroy Heroku APP."
  (interactive (list (heroku-get-app-name)))
  (if (yes-or-no-p (format "Are you sure you want to destroy %s?" (propertize app 'face 'warning)))
      (let ((confirmed-name (read-from-minibuffer (format "Type the name of the app to continue [%s]: " app))))
	(if (string= app confirmed-name)
	    (progn
	      (with-temp-message (format "Deleteing %s..." app)
		(->> (format "heroku apps:destroy -a %s --confirm %s" app app)
		     (shell-command)))
	      (message "%s destroyed." app)
	      (heroku-app-list-mode-refresh))
	  (message "Wrong app name. Cancelled.")))))

(defun heroku-get-app-config (app)
  "Get config for the APP."
  (interactive)
  (message (format "Getting app config for %s..." app))
  (->> (shell-command-to-string (format "heroku config -a %s" app))
       (s-split "\n")
       (-filter (lambda (s) (s-contains-p ":" s)))
       (-map (lambda (s) (s-split-up-to ":" s 1)))
       (-map (lambda (el) (list (s-trim (car el)) (s-trim-left (cadr el)))))))

(defun heroku-app-config-set (app key value)
  "In APP set KEY to VALUE."
  (message "Setting %s on %s..." key app)
  (let* ((result (->> (format "heroku config:set %s=%s -a %s" key value app)
		      call-process-shell-command)))
    (if (eq result 0)
	(message "%s" "Done.")
      (message "Something went wrong."))))

(defun heroku-app-config-unset (app key)
  "Unset (delete) config KEY in APP."
  (interactive (list heroku--app-name (car (heroku-get-config-kv))))
  (if (y-or-n-p (format "Are you sure you want to delete %s on %s?" key app))
      (progn
	(with-temp-message (format "Unsetting %s..." key)
	  (->> (format "heroku config:unset %s -a %s" key app)
	       call-process-shell-command))
	(heroku-app-config-refresh)
	(message "Done. App will restart."))))

(defun heroku-app-config-create (app key value)
  "Create config KEY with VALUE in APP."
  (interactive (list heroku--app-name (read-from-minibuffer "New key: ") (read-from-minibuffer "Value: ")))
  (if (alist-get key heroku--config-original nil nil 'string=)
      (progn
	(if (y-or-n-p (format "Key %s already exists.  Edit it instead?" key))
	    (progn
	      (heroku-config-edit)
	      (erase-buffer)
	      (insert value))))
    (if (y-or-n-p (format "Create %s=%s on %s?" key value app))
	(progn (heroku-app-config-set app key value)
	       (heroku-app-config-refresh)))))

(defun heroku-get-pipelines-list ()
  "Get list of Heroku pipelines."
  (with-temp-message "Getting Heroku pipelines..."
    (->> (shell-command-to-string "heroku pipelines")
	 (s-split "\n")
	 cdr
	 (-filter #'heroku-some-string-p))))

(defvar heroku-pipelines-mode-map
  (let* ((map_ (make-sparse-keymap)))
    (define-key map_ (kbd "a") 'heroku-pipelines-apps)
    (define-key map_ (kbd "?") 'heroku-pipelines-transient)
    map_)
  "Keymap for `heroku-pipelines-mode'.")

(transient-define-prefix heroku-pipelines-transient
  "Heroku help transient."
  [[:description "Heroku.el Pipelines"
		 ""]]
  [["Commands"
    ("a" "Apps" heroku-pipelines-apps)]]
  [["Heroku.el"
    ("?" "Help" heroku-pipelines-transient)
    ("q" "Quit" quit-window)]])

(define-derived-mode heroku-pipelines-mode tabulated-list-mode "Heroku Pipelines"
  "Heroku app list mode."
  (let ((columns [("My Pipelines" 100)])
	(rows (->> (heroku-get-pipelines-list)
		   (mapcar (lambda (x) `(nil [,x]))))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode)))

(defun heroku-pipelines-apps (pipeline)
  (interactive (list (heroku-get-app-name)))
  (with-temp-message (format "Getting apps for %s..." pipeline)
    (->> (shell-command-to-string (format "heroku pipelines:info %s" pipeline)))))

(comment
 (setq heroku-app-list nil)
 (setq tpp (heroku-pipelines-apps "ufybotmain"))
 (->> tpp))

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
    (define-key map_ (kbd "i") 'heroku-app-details)
    (define-key map_ (kbd "d") 'heroku-app-destroy)
    (define-key map_ (kbd "P") 'heroku-pipelines)
    map_)
  "Keymap for `heroku-app-list-mode'.")

(comment
 (setq tdata (heroku-get-app-list-2))
 (setq cdata (-map (lambda (el) (list (car el) (cadr el))) (car (heroku-get-app-list-2))))
 (setq tcols (apply 'vector cdata))
 (setq tlists (heroku-app-list-prepare-columns tdata))
 (-map (lambda (el) `(nil [,@el])) tlists)
 )

(defun heroku-app-list-prepare-columns (data)
  (let ((lists   (-map (lambda (el) (->> (-flatten (-map 'cddr el))
					 )) data)))
    (-map (lambda (el) `(nil [,@el])) lists)))

(define-derived-mode heroku-app-list-mode tabulated-list-mode "Heroku Apps"
  "Heroku app list mode."
  (unless heroku-app-list
    (heroku-refresh-app-list))
  (let* ((data heroku-app-list)
	 (column-data (-map (lambda (el) (list (car el) (cadr el))) (car data)))
	 (columns (apply #'vector column-data))
	 (rows  (heroku-app-list-prepare-columns data)))
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
    map_)
  "Keymap for `heroku-app-config-mode'.")

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

(define-derived-mode heroku-app-details-mode tabulated-list-mode "Heroku App Info"
  "Heroku app list mode."
  (let ((columns [("Description" 50) ("Value" 50)])
	(rows (->> (heroku-get-app-details heroku--app-name-details)
		   (mapcar (lambda (x) `(nil [,@x]))))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode)))

(defun heroku-app-config-refresh ()
  "Refresh app config."
  (interactive)
  (message "Refreshing %s config..." heroku--app-name)
  (setq heroku--config-original (heroku-get-app-config heroku--app-name))
  (heroku-app-config-mode)
  (message "Done. App will restart."))

(defun heroku-app-config (app)
  "Show environment variables for APP."
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
  "Read app name from app list."
  (aref (tabulated-list-get-entry) 0))

(defun heroku-get-app-name-propertized ()
  "Get app name and propertize it."
  (propertize (heroku-get-app-name) 'face 'transient-argument))

(defun heroku-get-logs (&optional args)
  "Get Heroku logs for app using ARGS."
  (interactive (list (transient-args 'heroku-logs-transient)))
  (let* ((app (heroku-get-app-name))
	 (buffer (format "*Heroku Logs: %s*" app)))
    (message (format "Gettings Heroku logs for %s..." app))
    (apply #'make-comint-in-buffer "heroku-logs" buffer "heroku" nil "logs" "-a" app args)
    (with-current-buffer buffer
      (heroku-logs-mode)
      (pop-to-buffer-same-window buffer))))

(transient-define-prefix heroku-logs-transient ()
  "Heroku logs transient."
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
  "Run a one-off process with COMMAND with ARGS in DETACHED mode inside heroku dyno."
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
  "Run COMMAND with ARGS in detached mode."
  (interactive (list (read-from-minibuffer "Command to run: ") (transient-args 'heroku-run-transient)))
  (heroku-run-command command args t))

(defun heroku-run-python (&optional args)
  "Run python on Heroku app with ARGS."
  (interactive (list (transient-args 'heroku-run-transient)))
  (heroku-run-command "python"))

(defun heroku-run-bash (&optional args)
  "Run bash on Heroku app with ARGS."
  (interactive (list (transient-args 'heroku-run-transient)))
  (heroku-run-command "bash"))

(defun heroku-get-config-kv ()
  "Get config key and value from list."
  (list (aref (tabulated-list-get-entry) 0)
	(aref (tabulated-list-get-entry) 1)))

(defvar heroku-env-edit-mode-map
  (let* ((map_ (make-sparse-keymap)))
    (define-key map_ (kbd "C-c '") 'heroku-config-edit-save)
    (define-key map_ (kbd "C-c C-k") 'heroku-config-edit-cancel)
    map_)
  "Keymap for `heroku-env-edit-mode'.")

(defun heroku-config-edit-cancel ()
  "Cancel editing Heroku config."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(defun heroku-config-edit-save ()
  "Save config to Heroku."
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
  (message "Press `C-c '` to save or `C-c C-k` to cancel"))

(defun heroku-config-edit ()
  "Edit Heroku config."
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
  "Heroku config transient."
  [[:description (lambda () (s-concat "Config for " heroku--app-name))
		 ""]]
  [["Commands"
    ("g" "Refresh" heroku-app-config-refresh)
    ("c" "Create" heroku-app-config-create)
    ("d" "Delete (unset)" heroku-app-config-unset)
    ("e" "Edit" heroku-config-edit)]])

(transient-define-prefix heroku-run-transient ()
  "Heroku run transient."
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
  "Heroku help transient."
  [[:description "Heroku.el commands"
		 ""]]
  [["Commands"
    ("g" "Refresh" heroku-app-list-mode-refresh)
    ("c" "Config" heroku-app-config)
    ("i" "Info" heroku-app-details)
    ("l" "Logs" heroku-logs-transient)
    ("r" "Run" heroku-run-transient)
    ("d" "Destroy" heroku-app-destroy)]
   ["Other modes"
    ("P" "Pipelines" heroku-pipelines)]]
  [["Heroku.el"
    ("?" "Help" heroku-help-transient)
    ("q" "Quit" quit-window)]])

(defun heroku--extract-app-details (s)
  "Extract app details from S output of heroku apps."
  (let ((name (s-match heroku-app-name-re s))
	(region (cdr (s-match heroku-region-re s)))
	(collab (s-match heroku-collab-re s)))
    (-flatten (list name (or region "us") (or collab "private")))))

(defun heroku-app-list-mode-refresh ()
  "Refresh Heroku app list."
  (interactive)
  (heroku-refresh-app-list)
  (heroku-app-list-mode))

(defun heroku-app-details ()
  "Start heroku.el and choose app to operate on."
  (interactive)
  (let* ((app (heroku-get-app-name))
	 (buff (format "*Heroku App Info: %s*" app)))
    (switch-to-buffer buff)
    (setq heroku--app-name-details app)
    (heroku-app-details-mode)))

;;;###autoload
(defun heroku-pipelines ()
  (interactive)
  (let ((buff "*Heroku Pipelines*"))
    (switch-to-buffer buff)
    (heroku-pipelines-mode)))

;;;###autoload
(defun heroku-list ()
  "Start heroku.el and choose app to operate on."
  (interactive)
  (let ((buff "*Heroku Apps*"))
    (switch-to-buffer buff)
    (heroku-app-list-mode)))

(provide 'heroku)
;;; heroku.el ends here
