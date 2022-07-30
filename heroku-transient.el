(require 'transient)
(require 's)

(defmacro heroku--comment (&rest body)
  "Ignores body, returns nil"
  nil)

(setq heroku--app-list nil)

(defun heroku--command (command &rest args)
  "Executes a Heroku CLI command and returns result as a string."
  (->> (reduce (lambda (s1 s2) (concat s1 " " s2)) (cons command args))
       (concat "heroku ")
       (shell-command-to-string)))

(defun heroku--list-apps ()
  "Return a list of all Heroku apps."
  (->> (heroku--command "list")
       (replace-regexp-in-string "===.+" "")
       (replace-regexp-in-string " (.+" "")
       (s-lines)
       (remove-if (lambda (s) (equal s "")))))

(defun heroku--domains (app-name)
  "Returns a plist of domains with two keys :defaul and :custom."
  (->> (heroku--command "domains" "-a" app-name)))

(defun heroku--update-app-list ()
  (interactive)
  (setq heroku--list-apps (heroku--list-apps)))

(define-transient-command heroku-menu ()
  "Heroku Transient")

(heroku-menu)




(heroku--comment
 (heroku--list-apps)
 (heroku--update-app-list)
 (heroku--domains "clj-blaster")

 "=== clj-blaster Heroku Domain
clj-blaster.herokuapp.com

=== clj-blaster Custom Domains
Domain Name        DNS Record Type DNS Target                                           SNI Endpoint     
shopify.blaster.ai CNAME           flat-starfish-p37oau9sqof66ixmaxwhkeq5.herokudns.com dryosaurus-00628 
"

 
 (setq dd '(:own "foo" :custom "bar"))
 (plist-get dd :own)

 
 "=== mb@m1k.pw Apps
aristarh-api (eu)
aristarh-api-staging (eu)
blaster-ms-api-users (eu)
blaster-ms-telegram-messenger (eu)
blaster-ms-template (eu)
closerbot (eu)
closertickets (eu)
kultzvukbot (eu)
monobank-ynab-api (eu)
rada-tracker (eu)
telegram-automations (eu)
uahconverter (eu)
ufetbot (eu)
woodynumbot (eu)
yogatherapybot (eu)

=== Collaborated Apps
teleteens-id (eu)  o.telegram.app@gmail.com
"
 
 (setq apps (heroku--list-apps))

 apps
 (heroku--command "list")
 (heroku--command "config" "-a" "bar" "spam")
 (heroku--command "config" "-a" "bar" "spam")
 (heroku--command "config" "-a" "bar" "spam")
 )

(defun heroku--list-apps ()
  (interactive)
  (shell-command-to-string (concat "heroku config -a " app-name)))
