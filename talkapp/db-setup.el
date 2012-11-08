;; db changes

;; A single db-change
(unless (db-get "20120629160609914467000" talkapp/db-change-db)
  (talkapp/org-new "thoughtworks"
                   :match-host "twirc.ferrier.me.uk"
                   :domain-name "thoughtworks.com"
                   :irc-server "irc.jbx.cc:6667"
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

;; Add the hn org
(unless (db-get "20120131130158357629000" talkapp/db-change-db)
  (talkapp/org-new "hn"
                   :match-host "hn.teamchat.net"
                   :domain-name "hn.teamchat.net"
                   :irc-server "irc.teamchat.net:6901"
                   :primary-channel "#hn")
  (db-put
   "20120131130158357629000"
   '(("timestamp" . "20120629160609914467000"))
   talkapp/db-change-db))

;; Add css to thoughtworks instance
(unless (db-get "20120008130011024002000" talkapp/db-change-db)
  (db-put
   "thoughtworks"
   (acons
    "css" "teamchat-thoughtworks.css"
    (db-get "thoughtworks" talkapp/org-db))
   talkapp/org-db)
  (db-put
   "20120008130011024002000"
   '(("timestamp" . "20120008130011024002000"))
   talkapp/db-change-db))

;; Add css to the HN instance
(unless (db-get "20121008141022056366000" talkapp/db-change-db)
  (db-put
   "hn"
   (acons
    "css" "teamchat-hn.css"
    (db-get "hn" talkapp/org-db))
   talkapp/org-db)
  (db-put
   "20121008141022056366000"
   '(("timestamp" . "20121008141022056366000"))
   talkapp/db-change-db))

;; Add the hacky team for the people at TW who helped me build teamchat
(unless (db-get "20121008141022056366000" talkapp/db-change-db)
  (db-put
   "hn"
   (acons
    "css" "teamchat-hn.css"
    (db-get "hn" talkapp/org-db))
   talkapp/org-db)
  (db-put
   "20121008141022056366000"
   '(("timestamp" . "20121008141022056366000"))
   talkapp/db-change-db))

(db-change talkapp/db-change-db "20124208144204733526000"
  (talkapp/org-new "teamtw"
                   :match-host "twirc.ferrier.me.uk"
                   :domain-name "teamtw.teamchat.net"
                   :irc-server "irc.jbx.cc:6667"
                   :primary-channel "#thoughtworks"))

(db-change talkapp/db-change-db "20121808151847747506000"
  (loop
     for (user . record)
     in (db-query talkapp/user-db '(= "org" "thoughtworks"))
     do (db-put
         user
         (setf (cdr (assoc "org" record)) "teamtw")
         talkapp/user-db)))

(db-change talkapp/db-change-db "20124208144204733526000"
  (talkapp/org-new "team"
                   :match-host "team.teamchat.net"
                   :domain-name "teamchat.net"
                   :irc-server "irc.teamchat.net:6601"
                   :primary-channel "#team"))

;; db-setup.el ends here
