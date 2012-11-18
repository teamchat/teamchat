;; Build the bits that need to be built

;; The emacs puppet stuff
(defun talkapp-build-puppet ()
  (interactive)
  (shell-command-to-string "cp -r ~/work/emacs-puppet puppet-emacs"))

(defconst shoes-off-elpakit
  '("~/work/shoes-off"
    "~/work/rcirc-ssh"
    "~/work/emacs-db"
    "~/work/esxml"
    "~/work/elnode-auth"
    "~/work/emacs-kv"
    "~/work/teamchat/talkapp"))

(defconst talkapp-do-rcirc nil)

;; The local elpa dir
(defun talkapp-do-elpakit-to-local ()
  (interactive)
  (elpakit "~/work/teamchat/shoesoff-elpa" shoes-off-elpakit))

;; the remote elpa
(defun talkapp-do-elpakit-to-live ()
  (interactive)
  (let ((remote
         "/ssh:nferrier@po1.ferrier:irc--talk/shoesoffaas/shoesoff-elpa/"))
    (delete-directory remote t)
    (elpakit remote shoes-off-elpakit)))

(defun talkapp-do-elpakit-to-livenet ()
  (interactive)
  ;; I have a special key and ssh config to make this work
  ;;
  ;;   Host www.teamchat.net
  ;;   User teamchat
  ;;   IdentityFile ~/.ssh/teamchatuser.pub
  (let ((remote
         "/ssh:www.teamchat.net:teamchat.net/shoesoff-elpa/"))
    (delete-directory remote t)
    (elpakit remote shoes-off-elpakit)))

;; Inside your emacs
(defun talkapp-do-elpakit-eval ()
  (interactive)
  (elpakit-eval shoes-off-elpakit))

;; End
