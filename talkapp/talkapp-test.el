;; just simple functions for mocking out bits of talkapp

(defun talkapp/user-db-init ()
  (let ((test-hash (make-hash-table :test 'equal)))
    (plist-put talkapp/user-db :db test-hash)
    ;; FIXME - these users both have the same tokens
    (let ((h1 (elnode--auth-make-hash "nic" "test"))
          (h2 (elnode--auth-make-hash "jim" "test")))
    (puthash "nic"
             `(("valid" . t)
               ("token" . ,h1)
               ("username" . "nic")
               ("password" . "test")
               ("email" . "nferrier@thoughtworks.com")
               ("key" . "ssh-dss AAAAB3NzaC test2"))
             test-hash)
    (puthash "jim"
             `(("valid" . t)
               ("token" . ,h2)
               ("username" . "jim")
               ("password" . "test")
               ("email" . "jim@test.com")
               ("key" . "ssh-dss AAAAB3 test1"))
             test-hash))))

;; FIXME - need values in DB
;;
;; for this one to work properly those two usernames need to be in the db
(defun talkapp/chat-list (channel)
  "Replacement for `talkapp/chat-list' that makes dummy chat."
  '(("2012-10-24 08:23:01:000001" "nic" "this is a test")
    ("2012-10-24 08:23:02:000001" "jim" "a conversation could occur")
    ("2012-10-24 08:23:03:000001" "nic" "replies would happen")
    ("2012-10-24 08:25:03:000001"
     "jim" "and even links to http://nic.ferrier.me.uk")
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
  (elnode-deferred-queue-process))

;; talkapp-test.el ends here
