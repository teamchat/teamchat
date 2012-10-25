;; tests for the talk/irc integration app

(defmacro talkapp/mock-db (&rest body)
  "Mock the swarm databases and eval BODY."
  (declare (debug (&rest form))
           (indent 0))
  `(let ((db-hash-do-not-save t))
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
                :filter talkapp/token-valid-get))))
       ,@body)))

(defun talkapp/test-make-user ()
  "Make a user."
  (talkapp/make-user
   talkapp-regform
   '(("username" . "testuser1")
     ("password" . "secret")
     ("email" . "test1@example.com")
     ("key" . "AFFSGSHhajsdkakdn"))))

(ert-deftest talk-start-session-config ()
  "Test the config abstraction."
  (talkapp/mock-db
    (talkapp/test-make-user)
    (let (connected
          (shoes-off--sessions ; mock to an empty
           (make-hash-table :test 'equal))
          (talkapp/irc-server-name "irc.example.com")
          (talkapp/irc-channels '("#test")))
      (flet ((shoes-off--get-config (username)
               (talk--get-shoes-off-config username))
             (rcirc-connect ; match the rcirc-connect arglist exactly
                 (server
                  &optional
                  port nick user-name
                  full-name startup-channels password encryption)
               (setq connected
                     (list server port nick user-name
                           full-name startup-channels password encryption))))
        (shoes-off-start-session "testuser1")
        (should
         (equal
          connected
          '("irc.example.com" 6667 "testuser1" "testuser1"
            "test1@example.com" ("#test") "secret" nil)))))))

(ert-deftest talkapp/shoes-off-auth ()
  (talkapp/mock-db
    (talkapp/test-make-user)
    (let ((talkapp/irc-server-name "irc.example.com")
          (talkapp/irc-channels '("#test")))
      (should
       (equal
        (list :username "testuser1"
              :password "secret"
              :server-alist
              `(("irc.example.com"
                 :nick "testuser1"
                 :port 6667
                 :user-name "testuser1"
                 :password "secret"
                 :full-name "test1@example.com"
                 :channels ("#test"))))
        (talkapp/shoes-off-auth "testuser1" "secret"))))))

(ert-deftest talkapp/shoes-off-valid ()
  (talkapp/mock-db
    (talkapp/test-make-user)
    (should-not (db-get "testuser1" talkapp/valid-token-db))
    (let ((user (db-get "testuser1" talkapp/user-db)))
      (db-put "testuser1" (acons "valid" t user) talkapp/user-db)
      (let ((v (db-get "testuser1" talkapp/valid-token-db)))
        (should v)))))

;; end
