;; just simple functions for mocking out bits of talkapp

(defun talkapp/user-db-init ()
  (interactive)
  (let ((test-hash (make-hash-table :test 'equal)))
    (plist-put talkapp/user-db :db test-hash)
    (talkapp/org-new
     "example-org"
     :match-host "example.teamchat.net"
     :domain-name "example.org"
     :irc-server "example-irc.teamchat.net"
     :primary-channel "#example")
    ;; FIXME - these users both have the same tokens
    (let ((h1 (elnode--auth-make-hash "nic" "test"))
          (h2 (elnode--auth-make-hash "caroline" "test"))
          (h3 (elnode--auth-make-hash "nic5" "test"))
          (h4 (elnode--auth-make-hash "bob" "test")))
      (puthash "nic"
               `(("valid" . t)
                 ("token" . ,h1)
                 ("username" . "nic")
                 ("org" . "UNKNOWN-ORG")
                 ("password" . "test")
                 ("email" . "nferrier@thoughtworks.com")
                 ("key" . "ssh-dss AAAAB3NzaC test2")) test-hash)
      (puthash "caroline"
               `(("valid" . t)
                 ("token" . ,h2)
                 ("username" . "caroline")
                 ("password" . "test")
                 ("org" . "example-org")
                 ("email" . "cjtapsell@tapsellferrier.co.uk")
                 ("key" . "ssh-dss AAAAB3 test1")) test-hash)
      (puthash "nic5"
               `(("valid" . t)
                 ("token" . ,h3)
                 ("username" . "nic5")
                 ("password" . "test")
                 ("org" . "example-org")
                 ("email" . "nic@ferrier.me.uk")
                 ("key" . "ssh-dss AAAAB3 test1")) test-hash)
      (puthash "bob"
               `(("valid" . t)
                 ("token" . ,h4)
                 ("username" . "bob")
                 ("password" . "test")
                 ("org" . "example-org")
                 ("email" . "bob@example.org")
                 ("key" . "ssh-dss AAAAB3 test1")) test-hash)
      ;; Setup the online cache as well
      (clrhash talkapp/online-cache)
      (puthash "cjtapsell@tapsellferrier.co.uk"
               :fakehttpcon
               talkapp/online-cache))))

;; FIXME - need values in DB
;;
;; for this one to work properly those two usernames need to be in the db
(defun talkapp/chat-list (channel)
  "Replacement for `talkapp/chat-list' that makes dummy chat."
  '(("2012-10-24 08:23:01:000001" "nic5" "this is a test")
    ("2012-10-24 08:23:02:000001" "caroline" "a conversation could occur")
    ("2012-10-24 08:23:03:000001" "nic5" "replies would happen")
    ("2012-10-24 08:25:03:000001"
     "caroline" "and even links to http://nic.ferrier.me.uk")
    ))

(defvar talkapp-comet-handler-doit t)

(defun talkapp/list-since (entered channel)
  (when talkapp-comet-handler-doit
    (setq talkapp-comet-handler-doit nil)
    '(("2012-10-24 08:23:06:0000001" "bob" "that would be fun!"))))

(defun talkapp/fake-defer ()
  "Run a fake comet thing."
  (interactive)
  (setq talkapp-comet-handler-doit t)
  (puthash "bob@example.org" :fakehttpcon talkapp/online-cache)
  (setq talkapp/user-state-changes
        '(("bob@example.org" . :online)))
  (puthash
   "nic@ferrier.me.uk" "cjtapsell@tapsellferrier.co.uk"
   talkapp/video-calls)
  (elnode-deferred-queue-process))

(defun talkapp/fake-offline ()
  "Fake offlining."
  (interactive)
  (setq talkapp/user-state-changes
        '(("bob@example.org" . :offline)))
  (elnode-deferred-queue-process))

(defun talkapp/fake-video ()
  "Fake video."
  (interactive)
  (puthash
   "nic@ferrier.me.uk" "cjtapsell@tapsellferrier.co.uk"
   talkapp/video-calls)
  (elnode-deferred-queue-process))

;; talkapp-test.el ends here
