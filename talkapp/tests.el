;; tests for the talk/irc integration app

(ert-deftest talkapp/irc-server->pair ()
  (should
   (equal
    (talkapp/irc-server->pair "irc.teamchat.net:6901")
    (list "irc.teamchat.net" 6901))))

(defmacro talkapp/mock-db (&rest body)
  "Mock the swarm databases and eval BODY."
  (declare (debug (&rest form))
           (indent 0))
  `(let ((db-hash-do-not-save t))
     ;; FIXME -This is a mess... would be great to find a way to
     ;; abstract db creation
     (let* ((talkapp/user-db
             (db-make
              `(db-hash
                :filename ,(format "/tmp/talk-user-db-%s" (uuid-string)))))
            (talkapp/email-valid-db
             (db-make
              `(db-hash
                :filename ,(format "/tmp/talk-email-db-%s" (uuid-string)))))
            (talkapp/keys-db
             (db-make
              `(db-hash
                :filename ,(format "/tmp/talk-keys-db-%s" (uuid-string)))))
            (talkapp/org-db
             (db-make
              `(db-hash
                :filename ,(format "/tmp/talk-org-db-%s" (uuid-string))))))
            ,@body)))

(defun talkapp/test-make-user ()
  "Make a user using the standard interface."
  (talkapp/make-user
   talkapp-regform
   '(("username" . "testuser1")
     ("password" . "secret")
     ("email" . "test1@example.com")
     ("key" . "AFFSGSHhajsdkakdn"))))

(ert-deftest talkapp/make-user ()
  "Test making a user."
  (talkapp/mock-db
    (talkapp/test-make-user)
    (should
     (equal
      (kvalist-sort
       (copy-list '(("username" . "testuser1")
                    ("org" . "UNKNOWN-ORG")
                    ("password" . "secret")
                    ("email" . "test1@example.com"))) 'kvcmp)
      (kvalist-sort
       (kvalist->filter-keys
        (db-get "testuser1" talkapp/user-db)
        "username" "org" "password" "email") 'kvcmp)))))

(defun* talkapp/test-make-user-and-org (&key
                                        username
                                        domain)
  "Make a user and an org using the standard interface."
  (let* ((user (or username "testuser2"))
         (dom (or domain "test.org"))
         (org-name (replace-regexp-in-string
                    "\\([^.]+\\)\\(\\.\\)\\(.*\\)"
                    "\\1-\\3" dom))
         (host-part (replace-regexp-in-string
                    "\\([^.]+\\)\\(\\.\\)\\(.*\\)"
                    "\\1\\3" dom))
         (host-name (concat host-part ".teamchat.net"))
         (irc-server (concat host-part "-irc.teamchat.net:6901"))
         (primary-channel (concat "#" host-part)))
    (talkapp/org-new
     org-name
     :match-host host-name
     :domain-name dom
     :irc-server irc-server
     :primary-channel primary-channel)
    (talkapp/make-user
     talkapp-regform
     `(("username" . ,user)
       ("password" . "secret")
       ("email" . ,(concat user "@" dom))
       ("key" . "AFFSGSHhajsdkakdn")))))

(ert-deftest talkapp/make-user-with-org ()
  "Test making a user."
  (talkapp/mock-db
    (talkapp/test-make-user-and-org)
    (should
     (equal
      (kvalist-sort
       (copy-list '(("username" . "testuser2")
                    ("org" . "test-org")
                    ("password" . "secret")
                    ("email" . "testuser2@test.org"))) 'kvcmp)
      (kvalist-sort
       (kvalist->filter-keys
        (db-get "testuser2" talkapp/user-db)
        "username" "org" "password" "email") 'kvcmp)))))

(ert-deftest talkapp/get-channel ()
  (talkapp/mock-db
   (talkapp/test-make-user-and-org)
   (should
    (equal
     "#testorg@testorg-irc.teamchat.net~testuser2"
     (talkapp/get-channel "testuser2")))))

(ert-deftest talkapp/irc-details ()
  (talkapp/mock-db
   (talkapp/test-make-user-and-org)
   (should
    (equal
     (list :username "testuser2"
           :password "secret"
           :server-alist
           (list
            (list "testorg-irc.teamchat.net"
                  :nick "testuser2"
                  :port 6901
                  :user-name "testuser2"
                  :password "secret"
                  :full-name "test2@test.org"
                  :channels (list "#testorg"))))
     (talkapp/irc-details
      "testuser2" "secret" "test2@test.org")))))

(ert-deftest talkapp/keys-ssh-auth ()
  (talkapp/mock-db
    (talkapp/test-make-user-and-org)
    (talkapp/key-add "testuser2" "test-key" "AAANDNBDb3223bdanbbdnad== nic")
    (should
     (equal
      (let ((talkapp-keys-program-home "/bin/ssh-irc"))
        (talkapp/keys-ssh-auth
         "testuser2"
         ;; just return the key-line
         (lambda (key-id key-line) key-line)))
      (concat
       "command=\"/bin/ssh-irc "
       "testuser2 testorg-irc.teamchat.net 6901 secret\","
       "permitopen=\"localhost:6901\" "
       "AAANDNBDb3223bdanbbdnad== nic testuser2:test-key")))))

(ert-deftest talkapp-start-session-config ()
  "Test the shoes-off config abstraction."
  (talkapp/mock-db
    (talkapp/test-make-user-and-org)
    (let (connected
          (shoes-off--sessions ; mock to an empty
           (make-hash-table :test 'equal)))
      (flet ((shoes-off/get-config (username)
               (talkapp/get-shoes-off-config username))
             (rcirc-connect ; match the rcirc-connect arglist exactly
                 (server
                  &optional
                  port nick user-name
                  full-name startup-channels password encryption)
               (setq connected
                     (list server port nick user-name
                           full-name startup-channels password encryption))))
        (shoes-off-start-session "testuser2")
        (should
         (equal
          connected
          '("testorg-irc.teamchat.net" 6901 "testuser2" "testuser2"
            "testuser2@test.org" ("#testorg") "secret" nil)))))))

(ert-deftest talkapp/shoes-off-auth ()
  (talkapp/mock-db
    (talkapp/test-make-user-and-org)
    (should
     (equal
      (list :username "testuser2"
            :password "secret"
            :server-alist
            `(("testorg-irc.teamchat.net"
               :nick "testuser2"
               :port 6901
               :user-name "testuser2"
               :password "secret"
               :full-name "testuser2@test.org"
               :channels ("#testorg"))))
      (talkapp/shoes-off-auth "testuser2" "secret")))))

(ert-deftest talkapp/org-new ()
  "Test the direcct creation of new organizations."
  (talkapp/mock-db
    (talkapp/org-new "test-org"
                     :match-host "testorg.teamchat.net"
                     :domain-name "test.org"
                     :irc-server "testorg.teamchat.net:6901"
                     :primary-channel "#testorg")
    (should
     (equal
      '(("name" . "test-org")
        ("host" . "testorg.teamchat.net")
        ("domain" . "test.org")
        ("irc-server" . "testorg.teamchat.net:6901")
        ("primary-channel" . "#testorg"))
      (db-get "test-org" talkapp/org-db)))))

(ert-deftest talkapp/get-my-org ()
  "Test the finding of orgs."
  (talkapp/mock-db
    (talkapp/org-new "test-org"
                     :match-host "testorg.teamchat.net"
                     :domain-name "test.org"
                     :irc-server "testorg.teamchat.net:6901"
                     :primary-channel "#testorg")
    (should
     (talkapp/get-my-org "nferrier@test.org"))
    (should
     (talkapp/get-my-org "nferrier@gmail.com" "testorg.teamchat.net"))
    (should-not
     (talkapp/get-my-org "nferrier@gmail.com" "www.teamchat.net"))))

(ert-deftest talkapp/people-list ()
  (talkapp/mock-db
    (talkapp/test-make-user-and-org)
    ;; Fake the online users
    (let ((talkapp/online-cache (make-hash-table :test 'equal)))
      (puthash "testuser2@test.org" :fakehttpcon talkapp/online-cache)
      (puthash "test1@example.com" :fakehttpcon talkapp/online-cache)
      ;; Now test
      (should
       (equal
        "<div id=\"emails\"><abbr title=\"testuser2@test.org\"/></div>"
        (talkapp/people-list "testuser2"))))))

(ert-deftest talkapp-email ()
  (flet ((message-send (&optional arg)
           (save-excursion
             (goto-char (point-min))
             (list
              (progn
                (re-search-forward
                 "To: \\(.+\\) <\\(.*\\)>"
                 (line-end-position)
                 t)
                (list
                 (match-string-no-properties 1)
                 (match-string-no-properties 2)))
              (progn
                (forward-line 1)
                (re-search-forward
                 "^http://.*"
                 nil
                 t)
                (match-string-no-properties 0))))))
    (talkapp/mock-db
     (talkapp/test-make-user-and-org)
     (let* ((user-rec (db-get "testuser2" talkapp/user-db))
            (email (aget user-rec "email"))
            (email-hash (sha1 (format "%s:%s" elnode-secret-key email))))
       (should
        (equal
         (list
          '("testuser2" "testuser2@test.org")
          (concat
           "http://testorg.teamchat.net/validate/"
           email-hash
           "/"))
         (talkapp/send-email user-rec email-hash)))))))

(ert-deftest talkapp/user-chat-add ()
  "Test the rcirc print-hooks stuff which effects webchat.

Some print hook logging:

  print hook > (localhost~nictest78) [nictest78] {PRIVMSG} [#nictest1] /boo!/
  print hook > (localhost~erwin) [nictest78] {PRIVMSG} [#nictest1] /boo!/

Into the print hook that is:

  process sender response target text

The response is nearly always PRIVMSG so we don't bother passing
that to this function we're testing here."
  ;; Fake the current-time so we can assert it
  (flet ((current-time ()
           (list 20664 27109 109940)))
    ;; Fake the chat store
    (let ((talkapp/user-chat (talkapp/hash)))
      (talkapp/user-chat-add
       "testuser1" "testuser2" "#testchannel" "some text!")
      (should
       (equal
        (list '((20664 27109 109940) "testuser2" "some text!"))
        (gethash "#testchannel" (gethash "testuser1" talkapp/user-chat))))
      (talkapp/user-chat-add
       "testuser1" "testuser3" "#testchannel" "more text!")
      (should
       (equal
        (list '((20664 27109 109940) "testuser3" "more text!")
              '((20664 27109 109940) "testuser2" "some text!"))
        (gethash "#testchannel" (gethash "testuser1" talkapp/user-chat)))))))

;; end
