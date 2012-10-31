;; tests for the talk/irc integration app

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
            (talkapp/auth-token-db
             (db-make
              `(db-filter
                :source ,talkapp/user-db
                :filter talkapp/db-filter-get)))
            (talkapp/valid-token-db
             (db-make
              `(db-filter
                :source ,talkapp/user-db
                :filter talkapp/token-valid-get)))
            (talkapp/email-valid-db
             (db-make
              `(db-hash
                :filename ,(format "/tmp/talk-email-db-%s" (uuid-string)))))
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
                    ("email" . "test1@example.com")
                    ("key" . "AFFSGSHhajsdkakdn"))) 'kvcmp)
      (kvalist-sort
       (kvalist->filter-keys
        (db-get "testuser1" talkapp/user-db)
        "username" "org" "password" "email" "key") 'kvcmp)))))

(defun talkapp/test-make-user-and-org ()
  "Make a user and an org using the standard interface."
  (talkapp/org-new
   "test-org"
   :match-host "testorg.teamchat.net"
   :domain-name "test.org"
   :irc-server "testorg-irc.teamchat.net:6901"
   :primary-channel "#testorg")
  (talkapp/make-user
   talkapp-regform
   '(("username" . "testuser2")
     ("password" . "secret")
     ("email" . "test2@test.org")
     ("key" . "AFFSGSHhajsdkakdn"))))

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
                    ("email" . "test2@test.org")
                    ("key" . "AFFSGSHhajsdkakdn"))) 'kvcmp)
      (kvalist-sort
       (kvalist->filter-keys
        (db-get "testuser2" talkapp/user-db)
        "username" "org" "password" "email" "key") 'kvcmp)))))

(ert-deftest talkapp/get-channel ()
  (talkapp/mock-db
   (talkapp/test-make-user-and-org)
   (should
    (equal
     "#testorg@localhost~testuser2"
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

(ert-deftest talkapp-start-session-config ()
  "Test the shoes-off config abstraction."
  (talkapp/mock-db
    (talkapp/test-make-user-and-org)
    (let (connected
          (shoes-off--sessions ; mock to an empty
           (make-hash-table :test 'equal)))
      (flet ((shoes-off--get-config (username)
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
            "test2@test.org" ("#testorg") "secret" nil)))))))

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
               :full-name "test2@test.org"
               :channels ("#testorg"))))
      (talkapp/shoes-off-auth "testuser2" "secret")))))

(ert-deftest talkapp/shoes-off-valid ()
  (talkapp/mock-db
    (talkapp/test-make-user)
    (should-not (db-get "testuser1" talkapp/valid-token-db))
    (let ((user (db-get "testuser1" talkapp/user-db)))
      (db-put "testuser1" (acons "valid" t user) talkapp/user-db)
      (let ((v (db-get "testuser1" talkapp/valid-token-db)))
        (should v)))))


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

;; end
