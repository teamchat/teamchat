;; db changes

;; A single db-change
(unless (db-get "20120629160609914467000" talkapp/db-change-db)
  (talkapp/org-new "thoughtworks"
                   :match-host "twirc.ferrier.me.uk"
                   :domain-name "thoughtworks.com"
                   :irc-server "irc.jbx.cc"
                   :primary-channel "#thoughtworks")
  (db-map (lambda (key value)
            (db-put
             key
             (acons "org" "thoughtworks" value)
               talkapp/user-db))
          talkapp/user-db)
  ;; Mark the change
  (db-put "20120629160609914467000"
          '(("timestamp" . "20120629160609914467000"))
          talkapp/db-change-db))

;; db-setup.el ends here
