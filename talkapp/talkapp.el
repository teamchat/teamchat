;;; talkapp.el --- make talking easy -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An app for the ssh/irc app.

;;; Code:

(elnode-app talkapp-dir
    erwin talkapp-templates esxml esxml-form web
    anaphora db db-pg kv uuid creole
    shoes-off network-stream)

(defgroup talkapp nil
  "An web and irc based application.

talkapp helps teams communicate. It offers a signup process to
provision IRC accounts, protects an IRC service with SSH and
provides a web interface to it as well."
  :group 'applications)

(defcustom talkapp-db-dir talkapp-dir
  "The directory used to store the talkapp databases."
  :group 'talkapp
  :type 'directory)

(defcustom talkapp-db-pg-instance "teamchat"
  "The PostgreSql instance used for pg databases."
  :group 'talkapp
  :type 'string)

(defcustom talkapp-db-pg-username "teamchat"
  "The PostgreSql username used for pg databases."
  :group 'talkapp
  :type 'string)

(defcustom talkapp-start-port 8100
  "The port to start talkapp on."
  :group 'talkapp
  :type 'integer)

(defcustom talkapp-video-server "localhost"
  "The server name of the media server."
  :group 'talkapp
  :type 'string)

(defcustom talkapp-keys-program-home
  "/home/teamchat/teamchat.net/ssh-irc"
  "The location of the program for doing irc connections."
  :group 'talkapp
  :type  'string)

(defcustom talkapp-ngircd-configs-dir
  "~/ngircd-configs"
  "The location of the ngircd configs."
  :group 'talkapp
  :type 'directory)

(defcustom talkapp-keys-file-name
  "~/.ssh/auth-keys"
  "The filename of the authorized keys."
  :group 'talkapp
  :type  'file)


(defconst talkapp/db-change-db
  (db-make
   `(db-hash
     :from-filename "/home/nferrier/work/teamchat/talkapp/db-change-db"
     :filename
     ,(expand-file-name
       (concat
        (file-name-as-directory talkapp-db-dir)
        "db-change-db"))))
  "META!!! The db of changes.

Each change is a nanosecond timestamp key with a single field
alist record:

  timestamp

eg: ((\"20120629160609914467000\"
      (\"timestamp\" . \"20120629160609914467000\")))

There are no other primitives for managing db-changes, though
there is the function `db-change-timestamp' which puts a useful
timestamp in the kill-ring.")

(defconst talkapp/org-db-file
  (db-make
   `(db-hash
     :from-filename "/home/nferrier/work/teamchat/talkapp/org-db"
     :filename
     ,(expand-file-name
       (concat
        (file-name-as-directory talkapp-db-dir)
        "org-db"))
     :query-equal kvassoc=))
  "The organization list.

This is THE OLD VERSION.

Contains the following fields:

  host             the web server hostname for the service, or nil
  name             the name of the organization
  domain           the domain name, last part of any email
  irc-server       the irc server to use
  primary-channel  main channel to join everyone in this org to.")

(defconst talkapp/org-db
  (db-make
   `(db-pg
     :db ,talkapp-db-pg-instance :username ,talkapp-db-pg-username
     :table "tc_org" :column "data" :key "name"))
  "Org database in the Postgres server.

Contains the following fields:

  host             the web server hostname for the service, or nil
  name             the name of the organization
  domain           the domain name, last part of any email
  irc-server       the irc server to use
  primary-channel  main channel to join everyone in this org to.")

(defconst talkapp/user-db
  (db-make
   `(db-hash
     :filename
     ,(expand-file-name
       (concat
        (file-name-as-directory talkapp-db-dir)
        "auth-db"))
     :query-equal kvassoc=))
  "The database where we store authentication details.")

(defconst talkapp/email-valid-db
  (db-make
   `(db-hash
     :filename
     ,(expand-file-name
       (concat
        (file-name-as-directory talkapp-db-dir)
        "email-db"))))
  "The database where we store validation details.

We key it by the validation key (a hash of email and secret key)
and store the username and the email.")

(defconst talkapp/invite-db
  (db-make
   `(db-hash
     :filename
     ,(expand-file-name
       (concat
        (file-name-as-directory talkapp-db-dir)
        "invite-db"))))
  "The database where we store invites.

What we store: ")

(defconst talkapp/keys-db
  (db-make
   `(db-hash
     :filename
     ,(expand-file-name
       (concat
        (file-name-as-directory talkapp-db-dir)
        "keys-db"))))
  "The database where we store ssh keys.")

(defconst talkapp/template-list
  (list
   (cons "head" talkapp-template/head-css)
   (cons "body-header" talkapp-template/body-header)
   (cons "footer" talkapp-template/body-footer))
  "List of template variables.")

(defconst talkapp-cookie-name "talkapp-user"
  "The name of the cookie we use for auth.")


;; Template function

(defvar talkapp/template-httpcon nil
  "Dynamically bindable HTTP connection for template function.")

(defun talkapp/template-std (&optional httpcon)
  "Return the standard template.

Mixes in org specific CSS if it can be found via HTTPCON or
`talkapp/template-httpcon'."
  (let ((org
         (awhen (or httpcon talkapp/template-httpcon)
           (let*
               ((host (elnode-http-host it :just-host t)))
             (cdar (db-query talkapp/org-db `(= "host" ,host)))))))
    (if org
        (append
         talkapp/template-list
         (list
          (when (aget org "css")
            (cons
             "host-head"
             (format
              "<link rel='stylesheet' type='text/css' href='/-/%s'/>"
              (assoc-default "css" org 'equal nil))))
          (cons "org-name" (concat (aget org "name") " "))))
        talkapp/template-list)))


;; Database utility function

(defun talkapp-list-ssh-keys (&optional make-buf)
  "Make an alist of the ssh-keys from the user database."
  (interactive (list t))
  (let ((keys (db-map
               (lambda (key user-data)
                 (when user-data
                   (kvdotassoc 'key user-data)))
               talkapp/user-db)))
    (if make-buf
        (with-current-buffer (get-buffer-create "*talkapp-keys*")
          (setq buffer-read-only t)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (goto-char (point-min))
            (loop for key in keys
               do (insert (format "%s -- %s\n" (cdr key) (car key))))
            (switch-to-buffer (current-buffer))))
        keys)))


(defun talkapp/port-next ()
  "Return the next avaiable port."
  (with-current-buffer
      (find-file-noselect
       (concat
        (file-name-as-directory talkapp-ngircd-configs-dir)
        ".ports"))
    (goto-char (point-max))
    (let ((new-port
           (+ 1 (if (re-search-backward "^Port \\([0-9]+\\)" nil t)
                    (string-to-number (match-string 1))
                    ;; Else use the first one
                    7002)))) ; we pre-allocated these
      (goto-char (point-max))
      (insert (format "Port %d\n" new-port))
      (save-buffer)
      new-port)))


;; Org function

(defun* talkapp/org-new (org-name
                         &key
                         match-host
                         domain-name
                         irc-server
                         primary-channel
                         css)
  "Make a new organisation record in the `talkapp/org-db'.

PRIMARY-CHANNEL should include the #."
  (assert (and (stringp org-name)
               (not (equal org-name ""))))
  (let ((record `(("name" . ,org-name)
                  ("host" . ,match-host)
                  ("domain" . ,domain-name)
                  ("irc-server" . ,irc-server)
                  ("primary-channel" . ,primary-channel))))
    (when css
      (setq record (acons "css" css record)))
    (db-put org-name record talkapp/org-db)))

(defun talkapp/get-my-org (email &optional http-host)
  "Get the organization for my EMAIL or HTTP-HOST.

Try EMAIL first (by pulling out the domain) and then HTTP-HOST."
  (let ((domain
         (when (string-match "^[^@]+@\\(.+\\)" email)
           (match-string 1 email))))
    (aget
     (car
      (kvalist->values
       (or
        (db-query talkapp/org-db `(= "domain" ,domain))
        (when http-host
          (db-query talkapp/org-db `(= "host" ,http-host))))))
     "name")))

(defun talkapp/get-org (username &optional field)
  "Return the org or the specific FIELD of the org."
  (let* ((record (db-get username talkapp/user-db))
         (org (aget record "org"))
         (org-record (db-get org talkapp/org-db)))
    (if field
        (aget org-record field)
        ;; Else return the whole org-record
        org-record)))

(defun talkapp/template-ngircd-conf (org-record)
  "Make an ORG-RECORD specific ngircd.conf.

Return the filename of the config file."
  (let* ((org-name (aget org-record "name"))
         (irc-server (aget org-record "irc-server"))
         (irc-server-pair (split-string irc-server ":"))
         (irc-port (cadr irc-server-pair))
         (conf-dir (file-name-as-directory talkapp-ngircd-configs-dir))
         (config-file (concat conf-dir org-name ".conf"))
         (motd-file (concat conf-dir org-name ".motd"))
         (pid-file (concat conf-dir org-name ".pid"))
         (server-name (concat org-name ".teamchat.net"))
         (info (format "The %s team ircd" org-name))
         (password (concat "secret" (number-to-string (random 1000))))
         ;; The template variable list
         (vars-alist
          `(("SERVERNAME" . ,server-name)
            ("INFO"       . ,info)
            ("PASSWORD"   . ,password)
            ("PORT"       . ,irc-port)
            ("MOTDFILE"   . ,motd-file)
            ("PIDFILE"    . ,pid-file))))
    (unless (file-exists-p conf-dir)
      (make-directory conf-dir t))
    (with-current-buffer (find-file-noselect motd-file)
      (insert "your new teamchat.net server is online!")
      (save-buffer))
    ;; Replace all the variables
    (with-current-buffer
        (find-file-noselect
         (concat
          (file-name-as-directory talkapp-dir)
          "ngircd-template.conf"))
      (goto-char (point-min))
      (while (re-search-forward "$\\([A-Z_-]+\\)" nil t)
        (let* ((variable (match-string 1))
               (value (aget vars-alist variable)))
          (when value
            (replace-match value t))))
      ;; Write the file, renames the buffer
      (write-file config-file)
      config-file)))

(defun talkapp/ngircd-proc-name (org-name)
  "Return the process name for the ORG-NAME."
  (format "*irc-server-%s*" org-name))

(defun talkapp/ngircd-boot (org-record)
  "Boot the irc server for the ORG."
  (let* ((conf-dir (file-name-as-directory talkapp-ngircd-configs-dir))
         (org-name (aget org-record "name"))
         (config-file (concat conf-dir org-name ".conf"))
         (proc-name (talkapp/ngircd-proc-name org-name)))
    ;; Make the config file if necessary
    (unless (file-exists-p config-file)
      (talkapp/template-ngircd-conf org-record))
    ;; Start the process if necessary
    (unless
        (and
         (get-process proc-name)
         (eq 'run (process-status (get-process proc-name))))
      (start-process
       proc-name proc-name
       "/usr/sbin/ngircd" "-f" (expand-file-name config-file) "-n"))))

(defun talkapp/robot-start (org-record)
  "Start the robot for the org."
  (let* ((irc-server-url (aget org-record "irc-server"))
         (irc-server-pair (split-string irc-server-url ":"))
         (irc-server (car irc-server-pair))
         (irc-port (cadr irc-server-pair))
         (primary-channel (aget org-record "primary-channel"))
         (rcirc-proc
          (talkapp-rcirc-connect
           irc-server irc-port
           ;; The robot's Nick, user-name and full-name should be configurable
           "erwin" "erwin" "Emacs Robot Within IRC Network"
           (list primary-channel)
           "" ; no one internally needs a password to connect to the ircd
           nil)))
    (process-put rcirc-proc :talkapp-erwin-org (aget org-record "name"))))

(defun talkapp-boot-org (org)
  "Boot the ircd for the ORG."
  (interactive
   (list (read-from-minibuffer "org-name: ")))
  (let* ((org-record (db-get org talkapp/org-db)))
    (talkapp/ngircd-boot org-record)
    (talkapp/robot-start org-record)))

(defun talkapp-make-org (org-name)
  "Make an org based on ORG-NAME.

Does everything from the ORG-NAME including: saving the org
record, allocating the port, making the ircd config and starting
the irc server."
  ;; FIXME should also allocate the robot
  (interactive
   (list (read-from-minibuffer "new org-name: ")))
  (let* ((match-host (format "%s.teamchat.net" org-name))
         (domain-name match-host)
         (irc-port (talkapp/port-next))
         (irc-server (format "irc.teamchat.net:%d" irc-port))
         (primary-channel (format "#%s" org-name))
         (org-record
          (talkapp/org-new org-name
                           :match-host match-host
                           :domain-name domain-name
                           :irc-server irc-server
                           :primary-channel primary-channel)))
    (talkapp-ngircd-boot (aget org-record "name"))
    org-record))

;; IRC setup stuff

(defun talkapp/irc-server->pair (server)
  (let ((ircd-lst (split-string server ":")))
    (list
     (car ircd-lst)
     (string-to-number (cadr ircd-lst)))))

(defun talkapp/irc-details (username password email)
  "Get the IRC details for the user specified.

We look up the organization in the db. If no organization is
present it's not possible to connect someone."
  (let* ((user-rec (db-get username talkapp/user-db))
         (org (aget user-rec "org"))
         (org-rec (db-get org talkapp/org-db))
         (irc-server-desc (aget org-rec "irc-server"))
         (channel (aget org-rec "primary-channel")))
    (destructuring-bind (irc-server irc-port)
        (talkapp/irc-server->pair irc-server-desc)
      (list :username username
            :password password
            :server-alist
            `((,irc-server
               :nick ,username
               :port ,irc-port
               :user-name ,username
               :password "secret"
               :full-name ,email
               :channels ,(list channel)))))))

;; Rcirc stuff

(defun talkapp/rcirc-name->username (rcirc-name)
  "Check RCIRC-NAME to see if it has a username.

Return the username if it does.

It also leaves the `match-data' for the RCIRC-NAME.  Sub-match 1
the irc server name and sub-match 2 is the username."
  (when (string-match "^\\([^~]+\\)~\\(.*\\)$" rcirc-name)
    (match-string-no-properties 2 rcirc-name)))

(defcustom talkapp-irc-server-name-map '()
  "A map of irc-server names to real names used to connect.

This is useful because we store logical irc server names in the
database but we want to actually connect to localhost (or
whatever) when we're testing.

This stores logical names against actual names, so for example
\"example.teamchat.net\" -> \"localhost\".  It can also be used
to store a default name mapping: \"*\".

See `talkapp/irc-server-name' for more details."
  :group 'talkapp
  :type 'sexp)

(defun talkapp/irc-server-name (name)
  "Opportunity to transform the irc-server NAME.

This is used to actually connect to the IRC server.

`talkapp-irc-server-name-map' is used to lookup the mapping.  If
there is a name match that is used, alternately \"*\* is
attempted.  If either produces a string the string is returned
else NAME is returned."
  (let ((override
         (or
          (aget talkapp-irc-server-name-map name)
          (aget talkapp-irc-server-name-map "*"))))
    (if override override name)))

(defun talkapp-rcirc-connect (server
                              &optional port nick user-name
                                full-name startup-channels password encryption)
  "Talk's irc connect function.

This ensures that bouncer sessions are namespaced with the user's
name."
  (let ((ons-func (symbol-function 'open-network-stream))
        ;; We don't use passwords to connect to the irc but ngircd
        ;; requires them
        (password ""))
    (flet ((open-network-stream
               (name buffer host service &rest parameters)
             ;; Override to ensure buffers are namespaced
             (let ((proc-name (concat name "~" user-name)))
               (funcall ons-func proc-name buffer host service parameters))))
      (rcirc-connect
       (talkapp/irc-server-name server) port
       nick user-name full-name
       startup-channels password encryption))))

(defun talkapp/hash ()
  (make-hash-table :test 'equal))

(defvar talkapp/user-chat (talkapp/hash)
  "List of user's with chat.

The structure is this:

  { username -> { channel -> list-of-updates } }

That is a hashtable of usernames to hashtables of channels to
lists of updates.")

(defvar talkapp/user-chat-gc-timer nil
  "Variable to hold the timer for the chat garbage collector.")

(defun talkapp/user-chat-add (username sender target text)
  "Add a record to the cache of updates."
  ;; add (list time-now sender text)
  ;; to the user's channel specific table
  (let ((channel-hash (gethash username talkapp/user-chat)))
    ;; Make the user's channel hash if it doesn't exist
    (unless channel-hash
      (setq channel-hash (talkapp/hash))
      (puthash username channel-hash talkapp/user-chat))
    ;; Get and update the user's list of changes
    (let ((changes (gethash target channel-hash)))
      (setq changes (cons (list (current-time) sender text) changes))
      (puthash target changes channel-hash))))

(defun talkapp/user-chat-gc ()
  "Try and garbage collect inside the chat list."
  (let* ((hours 2)
         (cut-off (time-subtract
                   (current-time)
                   (list 0 (* 60 (* 60 hours)) 0))))
    (maphash
     (lambda (username channelhash)
       (maphash
        (lambda (channelname chat-list)
          (let ((new-chat-list
                 (loop for (chat-time sender text) in chat-list
                    if (time-less-p chat-time cut-off)
                    return ret
                    collect (list chat-time sender text)
                    into ret
                    finally return ret)))
            (if new-chat-list
                (puthash channelname new-chat-list channelhash)
                ;; Else message and remove it
                (message
                 "talkapp/user-chat-gc clearing chat for %s %s"
                 username channelname)
                (remhash channelname channelhash))))
        channelhash))
     talkapp/user-chat)))

(defconst talkapp/print-hook-logging nil
  "Control to switch print hook logging on and off.")

(defun talkapp-rcirc-print-hook (process sender response target text)
  "The print hook gets everything printed in a channel."
  (when talkapp/print-hook-logging
    (message "print hook > (%s) [%s] {%s} [%s] /%s/"
             process sender response target text))
  (when (equal response "PRIVMSG")
    (cond
      ;; If the process is an end user it has this
      ((process-get process :shoes-off-channel-list)
       ;; Get the username, this is the process-name
       (let ((username (talkapp/rcirc-name->username (process-name process))))
         (flet ((deprop (s) (set-text-properties 0 (length s) nil s) s))
           (talkapp/user-chat-add
            username (deprop sender) (deprop target) (deprop text)))))
      ;; If the process is erwin it has this
      ((process-get process :talkapp-erwin-org)
       (message "talkapp-rcirc-print-hook erwin called with %s" text)
       (erwin-input process sender target text)
       ))))

(defun talkapp/rcirc-send (channel-buffer data)
  "Send DATA to CHANNEL-BUFFER."
  (with-current-buffer (get-buffer channel-buffer)
    (goto-char (point-max))
    (insert data)
    (rcirc-send-input)))


;; Shoes-off stuff

(defun talkapp-shoes-off-sync ()
  "Synchronize the sesions with the irc process state."
  (interactive)
  (clrhash shoes-off/sessions)
  (loop for (proc name)
     in (loop for p in (process-list) ; list of process
           collect (list p (process-name p)))
     ;; Filter to the irc ones
     if (talkapp/rcirc-name->username name)
     do
       (let ((server (match-string-no-properties 1 name))
             (username (match-string-no-properties 2 name)))
         (puthash
          (format "%s@%s" username server)
          proc
          shoes-off/sessions))))

(defun talkapp/get-shoes-off-config (username)
  "Return a db record in the form that can be used by shoes-off."
  (awhen (db-get username talkapp/user-db)
    (destructuring-bind (&key username password email)
        (kvplist->filter-keys
         (kvalist->plist it)
         :username :password :email)
      (talkapp/irc-details username password email))))

(defun talkapp/shoes-off-auth (username-spec password)
  "DB based implementation of `shoes-off/auth-check'.

We should expect USERNAME-SPEC to just be a username."
  (let* ((record (db-query talkapp/user-db `(= "username" ,username-spec)))
         (details (aget record username-spec)))
    (destructuring-bind (&key username password email token)
        (kvplist->filter-keys
         (kvalist->plist details)
         :username :password :email :token)
      (when (equal password (aget details "password"))
        (talkapp/irc-details username password email)))))

(defun talkapp/shoes-off-erwin-plugin (username args text shoes-off-con)
  "Implement recent history for the bouncer.

ARGS is the target, the channel or person being talked to.

SHOES-OFF-CON is the connection back to the bouncer user."
  (when (string-match "^erwin[ :]+\\(.*\\)" text)
    (let ((target args)
          (txt (match-string 1 text)))
      (flet ((send (sender str)
               (process-send-string
                shoes-off-con
                ;; This fakes the user's address badly
                (format ":%s!%s@localhost PRIVMSG %s :%s\r\n"
                        sender sender target str))))
        (cond
          ((equal txt "history")
           (loop for (chat-time sender sent-text)
              in (reverse
                  (gethash
                   target
                   (gethash username talkapp/user-chat)))
              do (send sender sent-text))
           (throw :shoes-off-escape-privmsg nil))
          ((string-match "^help\\( +\\([a-zA-Z]+\\)\\)*" txt)
           (let* ((sub (match-string 2 txt))
                  (sub-sym (when sub (intern sub))))
             (case sub-sym
               ('history
                (send "erwin" (concat "ask me 'history' and I will "
                                      "tell you the last 2 hours of history")))
               (t
                (send "erwin" "I am Erwin the Emacs Robot Within IRC Network.")
                (send "erwin" (concat "here are irc specific commands: "
                                      "history")))))))))))

(defun talkapp/rcirc-config ()
  "Only do the rcirc init if we need to.

If this variable is not bound or bound and t it will eval."
  (when (or (not (boundp 'talkapp-do-rcirc)) talkapp-do-rcirc)
    (add-hook 'rcirc-print-hooks 'talkapp-rcirc-print-hook)
    (setq shoes-off/get-config-plugin 'talkapp/get-shoes-off-config)
    (setq shoes-off/auth-plugin 'talkapp/shoes-off-auth)
    (setq shoes-off/rcirc-connect-plugin 'talkapp-rcirc-connect)
    (setq shoes-off-privmsg-plugin 'talkapp/shoes-off-erwin-plugin)))


;; Retrieval bits

(defun talkapp-cookie->user-name (httpcon)
  "Convert the cookie to a user-name."
  (let ((httpc (if httpcon
                   httpcon
                   elnode-replacements-httpcon)))
    (car
     (elnode-auth-cookie-decode
      (or
       (elnode-http-cookie httpc talkapp-cookie-name t))))))

(defun talkapp/get-user (&optional httpcon)
  "Get the user via the cookie on the HTTPCON."
  (db-get
   (talkapp-cookie->user-name httpcon)
   talkapp/user-db))

(defun talkapp/keyify (record)
  "Force RECORD to have string keys."
  (kvalist-keys->*
   record
   (lambda (key) (if (stringp key) key (symbol-name key)))))

(defun talkapp/get-user-http (&optional httpcon)
  "Get the user record but force string keys."
  (talkapp/keyify
   (talkapp/get-user httpcon)))

(defun talkapp/get-nick (email)
  "Get the user's NICK from the EMAIL."
  (aget
   (cdar (db-query talkapp/user-db `(= "email" ,email)))
   "username"))


;; Start the shoes-off session for this user

(defun talkapp/irc-server-start (org-name)
  "Start the irc server."
  (unless (eq 'run
              (process-status
               (get-process (talkapp/ngircd-proc-name org-name))))
    (talkapp/ngircd-boot org-rec)))

(defun talkapp/shoes-off-session-p (username &optional org-name)
  "Is the IRC session for the USERNAME established?

Passing an optional ORG-NAME saves a few db calls.

Returns the relevant session, if it's there, or `nil'."
  (let* ((org-rec (db-get
                   (or
                    org-name
                    (aget (db-get username talkapp/user-db) "org"))
                   talkapp/org-db))
         (irc-server-desc (aget org-rec "irc-server"))
         (irc-server-pair (split-string irc-server-desc ":"))
         (irc-server (car irc-server-pair))
         (irc-port (string-to-number (cadr irc-server-pair)))
         (session (let ((proc (gethash
                               (concat username "@" irc-server)
                               shoes-off/sessions)))
                    (and (processp proc)
                         (eq 'open (process-status proc)) proc))))
    session))

(defun talkapp-shoes-off-session (httpcon)
  "Manage sessions from the webapp."
  (with-elnode-auth httpcon 'talkapp-auth
    (let* ((username (talkapp-cookie->user-name httpcon))
           (user-rec (db-get username talkapp/user-db))
           (org-name (aget user-rec "org"))
           (org-rec (db-get org-name talkapp/org-db)))
      (if (not org-rec)
          (elnode-send-json httpcon (list :error "no irc registered for org"))
          ;; Else provision the server
          (let ((do-start (elnode-http-param httpcon "start"))
                (do-stop (elnode-http-param httpcon "stop"))
                (session (talkapp/shoes-off-session-p username org-name)))
            (cond
              ((and session do-stop)
               ;; Can't stop sessions right now
               (elnode-send-json httpcon (list :error "wont stop")))
              ((and session do-start)
               ;; Can't start sessions that are already started
               (elnode-send-json httpcon (list :error "already started")))
              ((and (not session) do-start)
               ;; Try and start the session ...
               ;; ... first test we have an irc process?
               (talkapp/irc-server-start org-name)
               (let ((started (shoes-off-start-session username org-name)))
                 (elnode-send-json httpcon (list :session started))))
              (t
               ;; Just return the status
               (elnode-send-json httpcon (list :session (and session t))))))))))

;; Chat webapp

(defun talkapp/time->str (emacs-time)
  (format-time-string "%Y-%m-%d-%H-%M-%S-%N" emacs-time))

(defun talkapp/user-chat-list (username channel)
  "Return the list of chats for the USERNAME and CHANNEL."
  (let ((channel-hash (gethash username talkapp/user-chat)))
    (when channel-hash
      (gethash channel channel-hash))))

(defun talkapp/user-chat-list-since (username channel since)
  "Pull the updates since SINCE from the CHANNEL of USERNAME."
  (let ((updates (talkapp/user-chat-list username channel)))
    (loop for (time-stamp sender text) in updates
       if (time-less-p since time-stamp)
       collect (list (talkapp/time->str time-stamp)
                     sender
                     text))))

(defun talkapp/get-irc-server (username)
  "Find the irc-server for USERNAME."
  (let* ((org (talkapp/get-org username))
         (ircd (aget org "irc-server"))
         (irc-server-name
          (progn
            (string-match "^\\([^:]+\\):[0-9]+" ircd)
            (match-string 1 ircd)))
         (irc-server (talkapp/irc-server-name irc-server-name)))
    irc-server))

(defun talkapp/get-channel (username &optional channel-name)
  "Given a USERNAME return the channel name.

CHANNEL-NAME may be specified, otherwise it is the user's primary
channel taken from the organisation record."
  (let* ((org (talkapp/get-org username))
         (channel (or channel-name (aget org "primary-channel")))
         (irc-server (talkapp/get-irc-server username)))
    (format "%s@%s~%s" channel irc-server username)))

(defun talkapp/get-ctrl-channel (username)
  "Given a USERNAME return the channel name."
  (let* ((org (talkapp/get-org username))
         (irc-server (talkapp/get-irc-server username)))
    (format "%s~%s" irc-server username)))

(defvar talkapp/online-cache
  (make-hash-table :test 'equal)
  "Key of emails to http connections.")

(defvar talkapp/httpcon-online-cache
  (make-hash-table :test 'equal)
  "Key of http-connections to emails.")

(defvar talkapp/user-state-changes nil
  "List of email and state as cons cells.

The state is either `:online' or `:offline'.")

(defun talkapp/people-list (username)
  "Template include for the people list.

Filters `talkapp/online-cache' to the people who are in the same
org as USERNAME."
  (let* ((org (aget (db-get username talkapp/user-db) "org"))
         online-list)
    (when org
      (maphash
       (lambda (key value)
         (let* ((rec (db-query talkapp/user-db `(= "email" ,key)))
                (key-org (aget (cdr (car rec)) "org")))
           (when (equal org key-org)
             (setq online-list
                   (append (list `(abbr ((title . ,key)))) online-list)))))
       talkapp/online-cache)
      (esxml-to-xml `(div ((id . "emails")) ,@online-list)))))

(defun talkapp/comet-user-state ()
  "List the users who have changed state."
  (when talkapp/user-state-changes
    (let ((to-send (copy-list talkapp/user-state-changes)))
      (setq talkapp/user-state-changes nil)
      (list :user to-send))))

(defvar talkapp/video-calls (make-hash-table :test 'equal)
  "Hash of emails being video called.

The value is a cons of the email of the caller and the time,
like:

  email-getting-the-call => (email-sending-the-call time).

The data here is only sent by `talkapp/comet-video-call'.")

(defun talkapp/comet-video-call (my-email)
  "Return any call indicator.

Returns the whole cons cell to the client.  The car is the
caller's email address, the cdr is the person being called which
is the person making this COMET request."
  (awhen (gethash my-email talkapp/video-calls)
    (remhash my-email talkapp/video-calls)
    (list :video (cons my-email (list (car it) (cdr it))))))

(defun talkapp/comet-interrupt (entered username channel my-email)
  "Produce a list of interrupts."
  (let ((msg-list (reverse
                   (talkapp/user-chat-list-since
                    username channel entered)))
        (user-list (talkapp/comet-user-state))
        (video (talkapp/comet-video-call my-email))
        to-send)
    (when msg-list
      (setq to-send (list :message msg-list)))
    (when user-list
      (setq to-send (append user-list to-send)))
    (when video
      (setq to-send (append video to-send)))
    to-send))

(defun talkapp-comet-handler (httpcon)
  "Defer until there is some change.

A change could be new chat messages, a new user online or offline
or a video call or some other action."
  (with-elnode-auth httpcon 'talkapp-auth
    (let* ((username (talkapp-cookie->user-name httpcon))
           (email (aget (db-get username talkapp/user-db) "email"))
           (default-channel (talkapp/get-org username "primary-channel"))
           (channel (let ((chan (elnode-http-param httpcon "channel")))
                      (if (equal (or chan "") "") default-channel chan)))
           (entered (current-time))
           (online (gethash username talkapp/online-cache)))
      ;; Mark the user online with the double hash
      (unless online
        (puthash httpcon email talkapp/httpcon-online-cache)
        (puthash email httpcon talkapp/online-cache))
      ;; Defer till the talk changes
      (elnode-defer-until
          (talkapp/comet-interrupt entered username channel email)
          (elnode-send-json httpcon elnode-defer-guard-it :jsonp t)))))

(defun talkapp/comet-fail-hook (httpcon state)
  "Remove the associated user from online state.

Updates `talkapp/user-state-changes' with the username.

Either `closed' or `failed' is the same for this purpose."
  (when (eq state 'failed)
    (let ((email (gethash httpcon talkapp/httpcon-online-cache)))
      (message "found %s in the online-cache, called with: %s" email state)
      (when email
        (remhash email talkapp/online-cache)
        (remhash httpcon talkapp/httpcon-online-cache)
        (setq talkapp/user-state-changes
              (append (list (cons email :offline))
                      talkapp/user-state-changes))))))

(defun talkapp-video-call-handler (httpcon)
  "Do a video call."
  (with-elnode-auth httpcon 'talkapp-auth
    (let* ((call-to (elnode-http-param httpcon "to")) ; email you're calling
           (time (elnode-http-param httpcon "time")) ; to unique the call
           (username (talkapp-cookie->user-name httpcon))
           (record (db-get username talkapp/user-db))
           (email (aget record "email")))
      (puthash call-to (cons email time) talkapp/video-calls)
      ;; Not sure about tbis for response - do I need the other email?
      (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
      (elnode-http-return httpcon "enjoy the call"))))

(defun talkapp-channel-messages (httpcon)
  "List the messages of the channel in JSON."
  (with-elnode-auth httpcon 'talkapp-auth
    (let* ((username (talkapp-cookie->user-name httpcon))
           (user-record (db-get username talkapp/user-db))
           (email (aget user-record "email"))
           (org-record (db-get (aget user-record "org") talkapp/org-db))
           (main-channel (aget org-record "primary-channel"))
           (channel (let ((chan (elnode-http-param httpcon "channel-name")))
                     (if (equal (or chan "") "") main-channel chan)))
           (an-hour (list 0 (* 60 60) 0))
           (since (time-subtract (current-time) an-hour))
           (messages (reverse
                      (talkapp/user-chat-list-since
                       username channel since))))
      ;; FIXME - we should mark the user online
      (elnode-send-json httpcon messages :jsonp t))))

(defun talkapp-channel-handler (httpcon)
  "Add a new channel.

If there are people selected then make the channel private."
  (with-elnode-auth httpcon 'talkapp-auth
    (elnode-method httpcon
      (GET
       (let* ((username (talkapp-cookie->user-name httpcon))
              (ctrl (talkapp/get-ctrl-channel username))
              (channels
               (let* ((user-rec (db-get username talkapp/user-db))
                      (org-name (aget user-rec "org")))
                 (unless (talkapp/shoes-off-session-p username org-name)
                   ;; Start the irc session
                   (shoes-off-start-session username))
                 (shoes-off-get-channels ctrl))))
         (elnode-send-json httpcon channels :jsonp t)))
      (POST
       (let* ((new-channel (elnode-http-param httpcon "channel"))
              (new-chan (format "#%s" new-channel))
              (username (talkapp-cookie->user-name httpcon))
              (ctrl (talkapp/get-ctrl-channel username))
              ;; This is a poor way to get the emails posted from the form.
              (nicks (loop
                        for param
                        in (kvalist->values
                            (elnode-http-params httpcon))
                        if (string-match "[^@]+@+.*" param)
                        collect (talkapp/get-nick param))))
         (talkapp/rcirc-send ctrl (format "/join %s" new-chan))
         (when nicks
           (talkapp/rcirc-send ctrl (format "/mode %s +s" new-chan))
           (talkapp/rcirc-send ctrl (format "/mode %s +i" new-chan))
           ;; Now send invites
           (loop for nick in nicks
              do (talkapp/rcirc-send
                  ctrl (format "/invite %s %s" nick new-chan))))
         (elnode-send-html
          httpcon "<html>thanks for that channel</html>"))))))

(defun talkapp-chat-add-handler (httpcon)
  "Send some chat to somewhere."
  (with-elnode-auth httpcon 'talkapp-auth
    (let* ((msg (elnode-http-param httpcon "msg"))
           (username (talkapp-cookie->user-name httpcon))
           (channel-name (elnode-http-param httpcon "channel-name"))
           (channel
            (if (equal channel-name "")
                (talkapp/get-channel username)
                channel-name)))
      (talkapp/rcirc-send channel msg)
      (elnode-send-html httpcon "<html>thanks for that chat</html>"))))

(defun talkapp/chat-templater ()
  "Return the chat page template variables."
  (let* ((httpcon elnode-replacements-httpcon)
         (username (talkapp-cookie->user-name httpcon))
         (record (db-get username talkapp/user-db))
         (email (aget record "email")))
    (append
     (list
      (cons
       "people"
       (talkapp/people-list username))
      (cons "my-email" email)
      (cons "my-nick" username)
      (cons "video-server" talkapp-video-server)
      (cons "chat-header" talkapp-template/chat-header))
     (talkapp/template-std httpcon))))

(defun talkapp-chat-handler (httpcon)
  "Handle the chat page."
  ;; FIXME - redirecct when chat not connected?
  (with-elnode-auth httpcon 'talkapp-auth
    (elnode-send-file
     httpcon (concat talkapp-dir "chat.html")
     :replacements 'talkapp/chat-templater)))

;; Not happy with this... want the username to be the key but not part
;; of the form
(defconst talkapp-keysform
  (esxml-form
    (:db talkapp/keys-db :db-key "name")
    (name :db-check (= "name" $)
          :check-failure
          ((:db-check "try adding your username to unique the key name")))
    (key :html :textarea
         :regex "[A-Za-z0-9-]+"
         :check-failure "just the main part of the key"))
  "Make a key form")

(defun talkapp/keys-ssh-auth (username fn)
  "Turn USERNAME keys into auth-keys lines into FN.

FN is called with the talkapp key id (which is
\"username:key-name\") and the key-line."
  (let* ((user-rec (db-get username talkapp/user-db))
         (org-rec (db-get (aget user-rec "org") talkapp/org-db))
         (key-rec (db-get username talkapp/keys-db))
         (password (aget user-rec "password")))
    (when org-rec
      ;; FIXME we need to write this for every key
      (destructuring-bind (server port)
          (talkapp/irc-server->pair (aget org-rec "irc-server"))
        (loop for (name . key-text) in key-rec
           return
             (let* ((key-id (concat username ":" name))
                    (key-line
                     (format
                      "command=\"%s %s %s %s %s\",permitopen=\"%s:%s\" %s %s"
                      talkapp-keys-program-home
                      username
                      ;; the command server and port
                      server
                      shoes-off-server-port
                      password
                      ;; the permit open server and port
                      "localhost"
                      shoes-off-server-port
                      key-text
                      key-id)))
               (funcall fn key-id key-line)))))))

(defun talkapp/keys-ssh-file (username)
  "Ensure USERNAME keys are written to the auth-file."
  (with-current-buffer (find-file-noselect talkapp-keys-file-name)
    (save-excursion
      (talkapp/keys-ssh-auth
       username
       (lambda (key-id key-line)
         (goto-char (point-min))
         (unless (re-search-forward (concat key-id "$") nil t)
           (goto-char (point-max))
           (insert key-line "\n")))))
    (save-buffer)))

(defun talkapp-keys-sync (&optional username)
  "Flush the keys database to the keys file."
  (interactive)
  (if username
      (talkapp/keys-ssh-file username)
      ;; Else do all of them
      (db-map
       (lambda (username value)
         (talkapp/keys-ssh-file username))
       talkapp/keys-db)))

(defun talkapp/key-add (username name text)
  "Add a new key to the db for USERNAME."
  (let* ((key-list (db-get username talkapp/keys-db))
         (new-keys  (acons name text key-list)))
    (db-put username new-keys talkapp/keys-db)
    new-keys))

(defun talkapp/key-save (username key-name key-text)
  "Add and save the key."
  (let ((keys (talkapp/key-add username key-name key-text)))
    (talkapp-keys-sync username)
    keys))

(defun talkapp-keys-handler (httpcon)
  "Return the user's keys."
  (with-elnode-auth httpcon 'talkapp-auth
    (let ((username (talkapp-cookie->user-name httpcon)))
      (elnode-method httpcon
        (GET
         (elnode-send-json
          httpcon
          (db-get username talkapp/keys-db):jsonp t))
        (POST
         (let* ((key-name (elnode-http-param httpcon "keyname"))
                (key-text (elnode-http-param httpcon "keytext"))
                (new-keys (talkapp/key-save username key-name key-text)))
           (elnode-send-json httpcon new-keys)))))))

;; user display

(defun talkapp/make-invites (username org)
  "Make 5 invites for USERNAME to send."
  (let* ((invites-max 1)
         (uuids
          (loop for i from 1 to invites-max
             collect (md5 (uuid-string))))
         res)
    (let ((hostname (aget org "host")))
      (loop for invite-id in uuids
         do
           (setq invite-url
                 (format
                  "http://%s/invite/%s"
                  hostname
                  invite-id))
           (db-put
            invite-url `(("id" . ,invite-url)("maker" . ,username))
            talkapp/invite-db)
         collect invite-url))))

(defun talkapp-user-handler (httpcon)
  "Present the main user page."
  (with-elnode-auth httpcon 'talkapp-auth
    (let* ((user-data (talkapp/get-user-http httpcon))
           (invite-data
            (talkapp/make-invites
             (aget user-data "username")
             (db-get (aget user-data "org") talkapp/org-db))))
      (elnode-send-file
       httpcon (concat talkapp-dir "user.html")
       :replacements
       (append
        user-data
        `(("invite-link"
           . ,(format
               (concat
                "<a title='copy this to invite others to teamchat' "
                "onclick='return false;' "
                "href='%s'>%s</a>")
               (car invite-data)
               (car invite-data))))
        (talkapp/template-std httpcon))))))


;; Registration stuff

(defun talkapp-auth-func (username)
  (let ((user (db-get username talkapp/user-db)))
    (when user
      (or
       ;; Either we're valid and we match a user page
       (and
        (string-match
         "/user/.*"
         (or
          (elnode-http-param elnode-auth-httpcon "redirect")
          (elnode-http-pathinfo elnode-auth-httpcon)))
        (aget user "valid")
        (aget user "token")) ; have to end with a token get
       ;; or we're registered
       (aget user "token")))))

(defun talkapp-validate-handler (httpcon)
  "Validates the user and logs them in."
  (with-elnode-auth httpcon 'talkapp-auth
    (let* ((hash (elnode-http-mapping httpcon 1))
           (record (db-get hash talkapp/email-valid-db))
           (username (aget record 'username))
           (user (db-get username talkapp/user-db))
           (user-page "/user/"))
      (db-put username (acons "valid" t user) talkapp/user-db)
      ;; Now log the user in and redirect them
      (elnode-auth-http-login
       httpcon
       username (aget user "password")
       user-page
       :cookie-name talkapp-cookie-name
       :auth-test 'talkapp-auth-func))))

(defun talkapp/make-user (regform data &optional http-host)
  "Make a user.

Optional HTTP-HOST is the host-name the call was sent to, if
present this is used to try to find an organization for the
user."
  (esxml-form-save
   regform data
   :db-data (lambda (data)
              (destructuring-bind (&key username password email organization)
                  (kvalist->plist data)
                ;; Add in the org
                (let ((org
                       (or (aget data "organization")
                           (talkapp/get-my-org email http-host))))
                  (acons
                   "org" (or org "UNKNOWN-ORG")
                   ;; Add in the auth token
                   (let* ((token
                           (elnode--auth-make-hash
                            username password)))
                     (acons "token" token data))))))))

(defun talkapp/save-reg (httpcon regform data)
  (let* ((http-host (elnode-http-header httpcon "Host"))
         (user (talkapp/make-user regform data http-host))
         (username (aget user "username"))
         (password (aget user "password")))
    (elnode-auth-http-login
     httpcon
     username password
     "/registered/"
     :cookie-name talkapp-cookie-name
     :auth-test 'talkapp-auth-func)))

(defconst talkapp-regform
  (esxml-form
    (:db talkapp/user-db :db-key "username")
    (username
     :regex "^[A-Za-z0-9-]+$"
     :db-check (= "username" $)
     :check-failure
     ((:regex "you need to use letters or digits to make a username")
      (:db-check "a user with that name exists")))
    (password :type :password :html :password)
    (email
     :type :email
     :db-check (= "email" $)
     :check-failure
     ((:email "not a valid email address?")
      (:db-check "a user with that email exists"))))
  "Registration form used for user's coming from an invite.")

(defconst talkapp-regform-org
  (esxml-form
    (:db talkapp/user-db :db-key "username")
    (username
     :regex "^[A-Za-z0-9-]+$"
     :db-check (= "username" $)
     :check-failure
     ((:regex "you need to use letters or digits to make a username")
      (:db-check "a user with that name exists")))
    (password :type :password :html :password)
    (organization
     :regex "^[A-Za-z0-9-]+$"
     :db-check (= "org" $)
     :check-failure
     ((:regex "you need to use letters or numbers to make an org name")
      (:db-check "An organization with that name exists - ask for an invite?")))
    (email
     :type :email
     :db-check (= "email" $)
     :check-failure
     ((:email "not a valid email address?")
      (:db-check "a user with that email exists"))))
  "Registration form used for users coming in the front door.")

(defun talkapp/send-email (user-record email-hash)
  "Send an email to the registrant."
  (let* ((username (aget user-record "username"))
         (email (aget user-record "email"))
         (org (aget  user-record "org"))
         (org-rec (db-get org talkapp/org-db))
         (host (aget org-rec "host")))
    (let ((sendmail-program "/usr/lib/sendmail")
          (smptmail-smtp-server "localhost")
          (smtpmail-smtp-service "smtp")
          (mail-host-address "teamchat.com")
          (mail-user-address "registration")
          (message-send-mail-function 'message-send-mail-with-sendmail)
          (send-message-p t))
      (message
       "sending reg mail to %s via %s including reg %s"
       username email email-hash)
      (cond
        ;; Test org
        ((string-match "^[a-z]+test[0-9]+" org)
         ;; Don't send a message at all
         (setq send-message-p nil))
        ;; Live launch stuff
        ((equal org "launch")
         (compose-mail
          (format "%s <%s>" username email) ; email
          (format "your teamchat.net account!")) ; subject
         (insert "thanks for registering on teamchat!\n")
         (insert "we will get back to you when we go live!\n")
         (insert "\nThanks!\nThe teamchat.net team"))
        ;; Normally we do this
        (t
         (compose-mail
          (format "%s <%s>" username email) ; email
          (format "validate your teamchat.net account!")) ; subject
         (insert "thanks for registering on teamchat!\n")
         (insert "to validate your email please click on the following link.\n")
         (insert
          (format "\nhttp://%s/validate/%s/\n\n" host email-hash))
         (insert "Thanks!\nThe teamchat.net team")))
      (when send-message-p
        (let ((ret (message-send)))
          (set-buffer-modified-p nil)
          (kill-buffer)
          ret)))))

(defun talkapp-registered-handler (httpcon)
  "The registered page.

Makes the validation hash, stores it in the db, queues the email
and directs you to validate."
  (with-elnode-auth httpcon 'talkapp-auth
    (let ((user (talkapp/get-user httpcon)))
      (if (aget user "valid")
          (elnode-send-redirect httpcon "/user/")
          (let* ((user-data (talkapp/keyify user))
                 (email (aget user-data "email"))
                 (username (aget user-data "username"))
                 ;; FIXME - this uses elnode's secret key - it would
                 ;; be better to use an app specific one
                 (email-hash (sha1 (format "%s:%s" elnode-secret-key email))))
            ;; Store the hash with the username and email address
            (db-put email-hash
                    `((username . ,username)(email . ,email))
                    talkapp/email-valid-db)
            ;; FIXME - We need to send an email!
            (talkapp/send-email user email-hash)
            ;; Send the file back
            (elnode-send-file
             httpcon
             (concat talkapp-dir "registered.html")
             :replacements (append user-data
                                   (talkapp/template-std httpcon))))))))

(defun talkapp-register-handler (httpcon)
  "Take a registration and create a user."
  (let ((esxml-field-style :bootstrap))
    (esxml-form-handle
     talkapp-regform-org httpcon (concat talkapp-dir "register.html")
     (lambda (data)
       (talkapp/save-reg httpcon talkapp-regform-org data))
     (talkapp/template-std httpcon))))

(defun talkapp-main-handler (httpcon)
  "The handler for the main page."
  (if-elnode-auth httpcon 'talkapp-auth
    (elnode-send-redirect httpcon "/registered/")
    (elnode-send-file
     httpcon
     (concat talkapp-dir "main.html")
     :replacements (talkapp/template-std httpcon))))

(defun talkapp-wiki-server (httpcon)
  "Wiki server used for /site/ pages."
  (let ((targetfile (elnode-http-mapping httpcon 1)))
    ;; Rip any final slash off
    (if (string-match ".*/$" targetfile)
        (setq targetfile (substring
                          targetfile
                          0 (- (length targetfile) 1))))
    ;; Mangle the mapping match to deal with creole files only
    (flet ((elnode-http-mapping (httpcon which)
             (concat targetfile ".creole")))
      ;; Do a docroot serve
      (elnode-docroot-for talkapp-dir
          with page
          on httpcon
          do
          (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
          (with-stdout-to-elnode httpcon
              (let ((creole-image-class "creole"))
                (creole-wiki
                 page
                 :destination t
                 :docroot talkapp-dir
                 :docroot-alias "/-/"
                 :css (list
                       (concat talkapp-dir "bootstrap.css")
                       (concat talkapp-dir "style.css"))
                 :body-header talkapp-template/body-header-wiki
                 :body-footer talkapp-template/body-footer)))))))

(defconst talkapp-access-log-name "talkapp"
  "The name of the access log.")

;; The authentication scheme.
(elnode-defauth 'talkapp-auth
  :auth-test 'talkapp-auth-func
  :cookie-name talkapp-cookie-name)

;;;###autoload
(defun talkapp-router (httpcon)
  "Main router."
  (let ((webserver (elnode-webserver-handler-maker talkapp-dir))
        (robots-txt (elnode-make-send-file (concat talkapp-dir "robots.txt")))
        (favicon (elnode-make-send-file (concat talkapp-dir "favicon.ico"))))
    (elnode-hostpath-dispatcher
     httpcon
     `(("^[^/]*//user/config/" . talkapp-irc-config-handler)
       ("^[^/]*//user/session/" . talkapp-shoes-off-session)
       ("^[^/]*//user/chat/" . talkapp-chat-handler)
       ("^[^/]*//user/send/" . talkapp-chat-add-handler)
       ("^[^/]*//user/channeldata/\\(.*\\)/*" . talkapp-channel-messages)
       ("^[^/]*//user/channel/$" . talkapp-channel-handler)
       ("^[^/]*//user/vidcall/" . talkapp-video-call-handler)
       ("^[^/]*//user/poll/" . talkapp-comet-handler)
       ("^[^/]*//user/keys/" . talkapp-keys-handler)
       ("^[^/]*//user/$" . talkapp-user-handler)
       ;; Reg stuff
       ("^[^/]*//register/" . talkapp-register-handler)
       ("^[^/]*//registered/" . talkapp-registered-handler)
       ("^[^/]*//validate/\\(.*\\)/" . talkapp-validate-handler)
       ("^[^/]*//-/\\(.*\\)$" . ,webserver)
       ("^[^/]*//favicon.ico$" . ,favicon)
       ("^[^/]*//robots.txt$" . ,robots-txt)
       ("^[^/]*//site/\\(.*\\)" . talkapp-wiki-server)
       ("^[^/]*//.*$" . talkapp-main-handler))
     :log-name talkapp-access-log-name
     :auth-scheme 'talkapp-auth)))

(defun talkapp-access-log-formatter (httpcon)
  "Make an access log line."
  (format
   "%s %60s %s"
   (elnode-log-access-format-func httpcon)
   (let ((cookie (elnode-http-cookie httpcon talkapp-cookie-name)))
     (or (cdr cookie) ""))
   (elnode-http-host httpcon :just-host t)))

;;;###autoload
(defun talkapp-start ()
  (interactive)
  (talkapp/rcirc-config)
  (setq elnode-log-access-alist
        (acons
         talkapp-access-log-name
         'talkapp-access-log-formatter
         elnode-log-access-alist))
  ;; Not sure if I should put these in or not.
  (setq elnode-error-log-to-messages nil)
  (setq revert-without-query
        (list (concat package-user-dir "/.*")))
  ;; Start the server
  (elnode-start 'talkapp-router
                :port talkapp-start-port
                :host "0.0.0.0")
  (add-hook 'elnode-defer-failure-hook 'talkapp/comet-fail-hook)
  (elnode-deferred-queue-start)
  (setq talkapp/user-chat-gc-timer
        (run-at-time "10 minutes" 600 'talkapp/user-chat-gc)))

(provide 'talkapp)

;;; talkapp.el ends here
