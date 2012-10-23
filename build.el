;; Build the bits that need to be built

;; The emacs puppet stuff
(shell-command-to-string "cp -r ~/work/emacs-puppet puppet-emacs")

;; The local elpa dir
(elpakit
 "~/work/shoes-off-aas/shoesoff-elpa"
 '("~/work/shoes-off-aas/talkapp"
   "~/work/shoes-off"
   "~/work/rcirc-ssh"
   "~/work/emacs-db"
   "~/work/esxml"
   "~/work/elnode-auth"
   "~/work/emacs-kv"))
