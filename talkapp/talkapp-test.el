;; just simple functions for mocking out bits of talkapp

(defun talkapp/user-db-init ()
  (interactive)
  (let ((test-hash (make-hash-table :test 'equal))
        (keys-test-hash (make-hash-table :test 'equal)))
    (plist-put talkapp/user-db :db test-hash)
    (plist-put talkapp/keys-db :db keys-test-hash)
    (talkapp/org-new
     "example-org"
     :match-host "example.teamchat.net"
     :domain-name "example.org"
     :irc-server "example-irc.teamchat.net:6667"
     :primary-channel "#example")
    (talkapp/org-new
     "nicferrierco"
     :match-host "nferrier.teamchat.net"
     :domain-name "nferrier.teamchat.net"
     :irc-server "nferrier-irc.teamchat.net:6668"
     :primary-channel "#nferrier")
    (talkapp/org-new
     "nicferrier-hn"
     :match-host "hn-nferrier.teamchat.net"
     :css ""
     :domain-name "hn-nferrier.teamchat.net"
     :irc-server "irc.teamchat.net:6651"
     :primary-channel "#hn-nferrier")
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
      ;; Some keys
      (puthash "nic5"
               '(("nics laptop" . "ssh-dss AAAAB3NzaC1kc3MAAACBANuM1SkfYrzGXYyg8bIqGvGMr6otpJQ3UEq6LdZDr0lQDLjV6YaAA+s2E/Vks9fCTwBzJ1y6wzEh5dVR2XCaxtMHcJTdBFBZNVnEUjx1mkuxaQb3LBWBlXrA/8ZfSC/eLaqwv7hVSbsZTm7AsO+fcp1O07YesnTOHer1EpmM4vuNAAAAFQD3s+lUUQzd8OMPyxM1b+xplGpMfwAAAIA5y/RpW5+xb3nsQha6YiJ8HSws7Vl92KV/oR0RUWwty4UgRhFr/6gQIZKX38Vp4JRbzflvFfCpA+7Bsupgsdd1GWI5NY199c8MnbXhKmnaKQHX2PSaEUdp5mePqYF7vj3lq1u3Ouuq8x+k9gn1PzzKK43nzn8JOtRMN0vpImmoCwAAAIEAlmKKcQNchCwDKvN/mUHaXplvm0vmv/Fkk4ZD0aIvii+wzRSSOFyuJ/oUVN98kXf2W9kgQDir6wnAhuU8PSZwKKyDwv3r0JVWm1XZkbUvoGYCXYEzufWy/D4thO2H0SUuoZ9RGdtSiEFyTSZH4bzUq2tGVibZuCPquDPl5AzRVIE= nferrier@tw-ubuntu")
                 ("nicskey" . "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJYjytqKomWFfwofbL7rX2Klit8HGPLdi8H5GiIvfc7ogcClrrHVbgLKgdzJy/9Qh4iSohkqNyLpB1y0Z2wJqRF+r7AdZpIEDj9T7jUg5fZiOproRmaPaxAJQzlat+ciTAQFfv0E0dZfaILb+sx3KnVvDmtZ08auXB494KNrJMMaJooa9ZG/Gf9eoQL7aSj0ugU8930wD2TPcjZuVhSZ0TLmbUugY3fxOJYzlhT/N4XoNnsIWSvxv2X0IbBJUsNTDl2DuxfLcX2Gsb2gxgswadSoFQJ6ieEKuiHgjwZnpfpyDTOnyGligyB9v4rdsU0XwQfKbcSG79ik19K5YTQ1Wz nferrier@nferrier"))
               keys-test-hash)
      ;; Setup the online cache as well
      (clrhash talkapp/online-cache)
      (puthash "cjtapsell@tapsellferrier.co.uk"
               :fakehttpcon
               talkapp/online-cache))))

;; FIXME - need values in DB
;;
;; for this one to work properly those two usernames need to be in the db
(defun talkapp/fake-chat-list (channel)
  "Replacement for `talkapp/chat-list' that makes dummy chat."
  '(("2012-10-24 08:23:01:000001" "nic5" "this is a test")
    ("2012-10-24 08:23:02:000001" "caroline" "a conversation could occur")
    ("2012-10-24 08:23:03:000001" "nic5" "replies would happen")
    ("2012-10-24 08:25:03:000001"
     "caroline" "and even links to http://nic.ferrier.me.uk")
    ))

(defvar talkapp-comet-handler-doit t)

(defun talkapp/fake-list-since (entered channel)
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
  ;; This fakes what happens with the http call from a user making a
  ;; call.
  (puthash
   "nic@ferrier.me.uk"
   (cons "cjtapsell@tapsellferrier.co.uk" "1352037650231")
   talkapp/video-calls)
  (elnode-deferred-queue-process))

;; talkapp-test.el ends here
