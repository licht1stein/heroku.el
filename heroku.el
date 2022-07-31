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

(defvar-local heroku-view-title nil)
(setq heroku-app-list nil)
(setq heroku-selected-app nil)
(setq heroku-target-email nil)

(defface heroku-selected-app-face
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "Face for selected app."
  :group 'heroku-faces)

(defface heroku-warning-face
  '((t :inherit warning))
  "Face for warnings."
  :group 'heroku-faces)

(defclass heroku-view--variable (transient-variable)
  ;; FIXME: We don't need :scope, but maybe a slot has to be defined.
  ((scope       :initarg :scope)))

(defun heroku--header ()
  (s-concat (propertize  "Selected app: " 'face 'transient-heading)
	    (if heroku-selected-app
		(propertize heroku-selected-app 'face 'heroku-selected-app-face)
	      (propertize  "No app selected, please select" 'face 'heroku-warning))
	    "\n"))

(defun heroku--header-w-target-user ()
  (s-concat (heroku--header)
	    (propertize "Target email: " 'face 'transient-heading)
	    (if heroku-target-email
		(propertize heroku-target-email 'face 'heroku-selected-app-face)
	      (propertize "No target user selectd, please select" 'face 'heroku-warning))
	    "\n"))


(defun heroku-logs-command (&optional args)
  "Connect and stream logs of a Heroku app."
  (interactive (list (transient-args 'heroku-logs-dispatch)))
  (let* ((buffer-name (format "*Heroku Logs: %s" heroku-selected-app)))
    (message (format "Gettings Heroku logs for %s..." heroku-selected-app))
    (apply #'make-comint-in-buffer `("heroku-logs" ,buffer-name "heroku" nil "logs" "-a" ,heroku-selected-app ,@args))
    (pop-to-buffer-same-window buffer-name)))

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

(heroku-access-defcommand heroku-access-add "access:add" 'heroku-access-dispatch)
(heroku-access-defcommand heroku-access-remove "access:remove" 'heroku-access-dispatch)
(heroku-access-defcommand heroku-access-update "access:update" 'heroku-access-dispatch)

(transient-define-prefix heroku-dispatch ()
  "Show Org QL View dispatcher."
  [[:description heroku--header
		 "App"
		 ("s" "Select target app" heroku-select-app)]]
  [["Commands"
    ("a" "Access" heroku-access-dispatch)
    ("l" "Logs" heroku-logs-dispatch)]])

(transient-define-prefix heroku-access-dispatch ()
  [[:description heroku--header-w-target-user
		 "Required"
		 ("s" "Select target app" heroku-select-app)
		 ("e" "Set target email" heroku-set-target-user)]]
  [["Arguments"
    ("-r" "git remote of app to use" "--remote=")
    ("-p" "list of permissions comma separated (deploy, manage, operate)" "--permissions=")
    ("-j" "output in json format" "--json")]]
  ;; [["Permissions"
  ;;   ("d" "deploy" "deploy")
  ;;   ("v" "view" "view")
  ;;   ("o" "operate" "operate")]]
  [["Execute"
    ("a" "access:add     add new user to your app" heroku-access-add)
    ("r" "access:remove  remove users from a team app" heroku-access-remove)
    ("a" "access:update  update existing collaborators on an team app" heroku-access-update)]])

(transient-define-prefix heroku-logs-dispatch ()
  :value '("--tail")
  [[:description heroku--header
		 "Required"
		 ("s" "Select target app" heroku-select-app)

		 "\nArguments"
		 ("-d" "only show output from this dyno type (web, worker)" "--dyno=")
		 ("-n" "number of lines to display" "--num=")
		 ("-r" "git remote of app to use" "--remote=")
		 ("-s" "only show output from this source (app, heroku)" "--source=")
		 ("-t" "continually stream logs" "--tail")]]
  [["Execute"
    ("l" "display log output" heroku-logs-command)]])


(transient-define-infix heroku-set-target-user ()
  :description "Set target user email"
  :class 'heroku-view--variable
  :variable 'heroku-target-email
  :argument ""
  :reader (lambda (prompt _initial-input history)
	    (setq heroku-target-email (read-from-minibuffer "Email: "))))

(transient-define-infix heroku-select-app ()
  :description "Select Heroku app to run commands against"
  :class 'heroku-view--variable
  :variable 'heroku-selected-app
  :argument ""
  :reader (lambda (prompt _initial-input history)
	    (if heroku-app-list
		(setq heroku-selected-app (completing-read "Select app: " heroku-app-list))
	      (if (y-or-n-p "App list is empty. Refresh?")
		  (progn
		    (message "Refreshing app list...")
		    (heroku-refresh-app-list)
		    (message "App list refreshed"))
		))))



(defun heroku-refresh-app-list ()
  "Refresh list of app available to Heroku CLI."
  (interactive)
  (message "Refreshing Heroku app list...")
  (setq heroku-app-list '("clj-country-news" "closerbot" "ufybot"))
  (message "Heroku app list refreshed"))


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
                         history)))


(provide 'heroku)
;;; heroku.el ends here
