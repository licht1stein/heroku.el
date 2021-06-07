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

(provide 'heroku)
