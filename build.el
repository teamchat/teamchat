;; Build the bits that need to be built

;; The emacs puppet stuff
(shell-command-to-string "cp -r ~/work/emacs-puppet puppet-emacs")

(defconst shoes-off-elpakit
  '("~/work/shoes-off"
    "~/work/rcirc-ssh"
    "~/work/emacs-db"
    "~/work/esxml"
    "~/work/elnode-auth"
    "~/work/emacs-kv"
    "~/work/shoes-off-aas/talkapp"))

(defconst talkapp-do-rcirc nil)

;; The local elpa dir
(elpakit "~/work/shoes-off-aas/shoesoff-elpa" shoes-off-elpakit)

(elpakit-eval shoes-off-elpakit)

;; End
