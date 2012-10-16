;; tests for the talk/irc integration app

(defmacro talk--mock-db (&rest body)
  "Mock the swarm databases and eval BODY."
  (declare (debug (&rest form))
           (indent 0))
  `(let ((db-hash-do-not-save t))
     (let* ((talk--user-db
             (db-make
              `(db-hash
                :filename ,(format "/tmp/talk-user-db-%s" (uuid-string)))))
            (talk--auth-token-db
             (db-make
              `(db-filter
                :source ,talk--user-db
                :filter talk--db-filter-get))))
       ,@body)))

(defun talk--test-make-user ()
  "Make a user."
  (talk--make-user
   talk-regform
   '(("username" . "testuser1")
     ("password" . "secret")
     ("email" . "test1@example.com")
     ("key" . "AFFSGSHhajsdkakdn"))))

(ert-deftest talk-start-session-config ()
  "Test the config abstraction."
  (talk--mock-db
    (talk--test-make-user)
    (let (connected
          (shoes-off--sessions ; mock to an empty
           (make-hash-table :test 'equal))
          (talk--irc-server-name "irc.example.com")
          (talk--irc-channels '("#test")))
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

(ert-deftest talk--shoes-off-auth ()
  (talk--mock-db
    (talk--test-make-user)
    (let ((talk--irc-server-name "irc.example.com")
          (talk--irc-channels '("#test")))
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
        (talk--shoes-off-auth "testuser1" "secret"))))))

;; end
