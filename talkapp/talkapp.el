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
    anaphora esxml esxml-form db kv uuid
    shoes-off rcirc-ssh network-stream)

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

(defconst talkapp/db-change-db
  (db-make
   `(db-hash
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

(defconst talkapp/org-db
  (db-make
   `(db-hash
     :filename
     ,(expand-file-name
       (concat
        (file-name-as-directory talkapp-db-dir)
        "org-db"))))
  "The organization list.

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
        "auth-db"))))
  "The database where we store authentication details.")

(defun talkapp/db-filter-get (key value)
  (aget value "token"))

(defconst talkapp/auth-token-db
  (db-make
   `(db-filter
     :source ,talkapp/user-db
     :filter talkapp/db-filter-get))
  "Token view of the user db.")

(defun talkapp/token-valid-get (key value)
  (when (aget value "valid")
    (aget value "token")))

(defconst talkapp/valid-token-db
  (db-make
   `(db-filter
     :source ,talkapp/user-db
     :filter talkapp/token-valid-get))
  "Token/valid view of the user db.")

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


;; Org function

(defun* talkapp/org-new (org-name
                         &key
                         match-host
                         domain-name
                         irc-server
                         primary-channel)
  "Make a new organisation record in the `talkapp/org-db'.

PRIMARY-CHANNEL should include the #."
  (assert (and (stringp org-name)
               (not (equal org-name ""))))
  (let ((record `(("name" . ,org-name)
                  ("host" . ,match-host)
                  ("domain" . ,domain-name)
                  ("irc-server" . ,irc-server)
                  ("primary-channel" . ,primary-channel))))
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
  (let* ((record (db-get username talkapp/user-db))
         (org (aget record "org"))
         (org-record (db-get org talkapp/org-db)))
    (if field
        (aget org-record field)
        ;; Else return the whole org-record
        org-record)))


;; Auth cookie constants

(defconst talkapp-session-cookie-name "talkapp-session"
  "The name of the cookie we use for email-valid auth.")

(defconst talkapp-cookie-name "talkapp-user"
  "The name of the cookie we use for auth.")


;; IRC setup stuff

(defun talkapp/irc-details (username password email)
  "Get the IRC details for the user specified.

We look up the organization in the db. If no organization is
present it's not possible to connect someone."
  (let* ((user-rec (db-get username talkapp/user-db))
         (org (aget user-rec "org"))
         (org-rec (db-get org talkapp/org-db))
         (irc-server-desc (aget org-rec "irc-server"))
         (irc-server-pair (split-string irc-server-desc ":"))
         (irc-server (car irc-server-pair))
         (irc-port (string-to-number (cadr irc-server-pair)))
         (channel (aget org-rec "primary-channel")))
    (list :username username
          :password password
          :server-alist
          `((,irc-server
             :nick ,username
             :port ,irc-port
             :user-name ,username
             :password "secret"
             :full-name ,email
             :channels ,(list channel))))))

;; Rcirc stuff

(defvar talkapp/rcirc-connect-with-ssh t
  "Connect with ssh or not.")

(defun talkapp-rcirc-connect (server
                              &optional port nick user-name
                                full-name startup-channels password encryption)
  "Talk's irc connect function.

This ensures that bouncer sessions are namespaced with the user's
name."
  (let ((ons-func (symbol-function 'open-network-stream)))
    (flet ((open-network-stream
               (name buffer host service &rest parameters)
             ;; Override to ensure buffers are namespaced
             (let ((proc-name (concat name "~" user-name)))
               (funcall ons-func proc-name buffer host service parameters)))
           (rcirc-ssh--get-key (server port nick user-name)
             ;; Override ssh key finding to find the key for the user
             (expand-file-name (format "~/ircdkeys/%s" user-name))))
      (if talkapp/rcirc-connect-with-ssh
          (rcirc-ssh-connect server
                             port
                             nick
                             user-name
                             full-name
                             startup-channels
                             password
                             encryption)
          ;; else do without ssh
          (rcirc-connect server
                         port
                         nick
                         user-name
                         full-name
                         startup-channels
                         password
                         encryption)))))

;; Shoes-off stuff

(defun talkapp/get-shoes-off-config (username)
  "Return a db record in the form that can be used by shoes-off."
  (awhen (db-get username talkapp/user-db)
    (destructuring-bind (&key username password email)
        (kvplist->filter-keys
         (kvalist->plist it)
         :username :password :email)
      (talkapp/irc-details username password email))))

(defun talkapp/shoes-off-auth (username-spec password)
  "DB based implementation of `shoes-off--auth-check'.

We should expect USERNAME-SPEC to just be a username."
  (let* ((record (db-query talkapp/user-db `(= "username" ,username-spec)))
         (details (aget record username-spec)))
    (destructuring-bind (&key username password email key token)
        (kvplist->filter-keys
         (kvalist->plist details)
         :username :password :email :key :token)
      (when (equal password (aget details "password"))
        (talkapp/irc-details username password email)))))

(defun talkapp/rcirc-config ()
  "Only do the rcirc init if we need to.

If this variable is not bound or bound and t it will eval."
  (when (or (not (boundp 'talkapp-do-rcirc)) talkapp-do-rcirc)
    (setq shoes-off--get-config-plugin 'talkapp/get-shoes-off-config)
    (setq shoes-off--auth-plugin 'talkapp/shoes-off-auth)
    (setq shoes-off--rcirc-connect-plugin 'talkapp-rcirc-connect)
    ;; Ensures the time format support us pulling back accurately
    (setq rcirc-time-format "%Y-%m-%d %H:%M:%S:%N ")))


;; Retrieval bits

(defun talkapp-cookie->user-name (httpcon)
  "Convert the cookie to a user-name."
  (let ((httpc (if httpcon
                   httpcon
                   elnode-replacements-httpcon)))
    (car
     (elnode-auth-cookie-decode
      (or
       (elnode-http-cookie httpc talkapp-session-cookie-name t)
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
  "Get the user but force string keys."
  (talkapp/keyify
   (talkapp/get-user httpcon)))


;; Provision the ircd with this user

(defun talkapp-irc-config-handler (httpcon)
  "Run the ircd provisioning script."
  (with-elnode-auth httpcon 'talkapp-session
    (let ((username (talkapp-cookie->user-name httpcon)))
      (if (file-exists-p
           (expand-file-name (concat "~/ircdkeys/" username)))
          ;; If the key exists then don't do it again
          (elnode-send-status httpcon 204)
          ;; else make the key
          (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
          (if (or (not (boundp 'talkapp-do-rcirc)) talkapp-do-rcirc)
              (elnode-child-process
               httpcon
               "bash" "/home/emacs/ircdmakeuser" username)
              (elnode-send-json httpcon (list :error t)))))))

;; Start the shoes-off session for this user

(defun talkapp-shoes-off-session (httpcon)
  "Manage sessions from the webapp."
  (with-elnode-auth httpcon 'talkapp-session
    (let* ((username (talkapp-cookie->user-name httpcon))
           (user-rec (db-get username talkapp/user-db))
           (org (aget user-rec "org"))
           (org-rec (db-get org talkapp/org-db))
           (irc-server-desc (aget org-rec "irc-server"))
           (irc-server-pair (split-string irc-server-desc ":"))
           (irc-server (car irc-server-pair))
           (irc-port (string-to-number (cadr irc-server-pair)))
           ;; FIXME hard coded server for now
           (session (gethash
                     (concat username "@" irc-server)
                     shoes-off--sessions))
           (do-start (elnode-http-param httpcon "start"))
           (do-stop (elnode-http-param httpcon "stop")))
      (cond
        ((and session do-stop)
         ;; Can't stop sessions right now
         (elnode-send-json httpcon (list :error "wont stop")))
        ((and session do-start)
         ;; Can't start sessions that are already started
         (elnode-send-json httpcon (list :error "already started")))
        ((and (not session) do-start)
         ;; Start the session
         (shoes-off-start-session username)
         (elnode-send-json httpcon (list :session t)))
        (t
         ;; Just return the status
         (elnode-send-json httpcon (list :session (and session t))))))))

;; Chat webapp

(defun talkapp/timestamp->time (source-time)
  "Convert a timestamp to a time."
  (let* ((encodeable-time
          (progn
            (string-match
             (concat
              "\\([0-9]+-[0-9]+-[0-9]+\\) " ; date part
              "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\):" ; time part
              "\\(.*\\)") ; nano-seconds
             source-time)
            (concat
             (match-string 1 source-time) " "
             (match-string 2 source-time) ":"
             (match-string 3 source-time) ":"
             (match-string 4 source-time))))
         (nano-seconds (string-to-number (match-string 5 source-time))))
    (append
     (apply
      'encode-time
      (parse-time-string encodeable-time))
     (list nano-seconds))))

(defun talkapp/list-since (time buffer-name)
  "Get the list of chats since TIME from BUFFER-NAME."
  (let ((time-to-find time)
        (doit t)
        (irc-line-re (concat
                      "^\\([0-9]+-[0-9]+-[0-9]+ " ; date part
                      "[0-9]+:[0-9]+:[0-9]+:[0-9]+\\) " ; time part
                      "<\\([^>]+\\)> +\\(.*\\)"))) ; the rest
    (with-current-buffer (get-buffer buffer-name)
      (save-excursion
        (goto-char (point-max))
        ;; Find the first point greater than the time-to-find
        (while doit
          (if (not (re-search-backward irc-line-re nil t))
              ;; No match - stop straight away
              (setq doit nil)
              ;; Else we have a match so check the time
              ;;; (match-string-no-properties 0) ; useful for debug
              (setq doit
                    (time-less-p
                     time-to-find
                     (talkapp/timestamp->time
                      (condition-case nil
                          (match-string-no-properties 1)
                        (error "1970-01-01 00:00:00:000000")))))))
        (loop while (progn
                      (forward-line 1)
                      (re-search-forward irc-line-re nil t))
           collect
             (list
              (match-string-no-properties 1)
              (match-string-no-properties 2)
              (get-text-property
               (next-single-property-change
                (line-beginning-position)
                'rcirc-text)
               'rcirc-text)))))))

(defun talkapp/list-since-mins-ago (minutes buffer-name)
  "Return all the chat since MINUTES ago."
  (let* ((time-to-find
          (time-subtract (current-time)
                         (seconds-to-time (* minutes 60)))))
    (talkapp/list-since time-to-find  buffer-name)))

(defun talkapp/date->id (date)
  "Convert DATE to a form usable as an HTML ID."
  (subst-char-in-string
   ?: ?-
   (apply 'format "%s-%s" (split-string date))))

(defun talkapp/entry->html (date username message)
  "Return the templated form of the row."
  (let ((email
         ;; FIXME this is ONLY because thoughtworks ran it's own irc
         ;; with people registered out of band of the webapp
         (aif (db-get username talkapp/user-db)
             (aget it "email")
           "unknown@thoughtworks.com")))
    `(tr
      ((id . ,(talkapp/date->id date)))
      (td
       ((class . ,(concat "username " username)))
       (abbr
        ((title . ,email))
        ,username))
      (td
       ((class . "message"))
       ,message))))

(defconst talkapp/default-chat-history-minutes 60)

(defun talkapp/chat-list (channel)
  "Make a list of the CHANNEL chatter."
  (reverse
   (talkapp/list-since-mins-ago
    talkapp/default-chat-history-minutes
    channel)))

(defun talkapp/get-channel (username)
  "Given a USERNAME return the channel name."
  ;; FIXME!!! the localhost bit is only when we are connecting via
  ;; ssh/irc. So it should be adapted via db settings for the user.
  (format
   "%s@localhost~%s"
   (talkapp/get-org username "primary-channel")
   username))

(defun talkapp/list-to-html (username)
  "Return the list of chat as rows for initial chat display."
  (let* ((channel (talkapp/get-channel username))
         (chat-list (talkapp/chat-list channel)))
    (if chat-list
        (loop for entry in chat-list
           if (equal 3 (length entry))
           concat
             (esxml-to-xml
              (apply 'talkapp/entry->html entry)))
        ;; Else send some appropriate xml
        (esxml-to-xml '(div ((id . "empty-chat")) "no chat")))))

(defun talkapp/since-list->htmlable (since-list)
  "Convert SINCE-LIST into something with HTML IDs."
  (loop
     for (date username msg) in since-list
     collect
       (list (talkapp/date->id date) username msg)))

(defun talkapp/comet-since-list (entered channel)
  "Comet form of `talkapp/since-list'."
  (let ((lst (talkapp/list-since entered channel)))
    (when lst
      (list :message (talkapp/since-list->htmlable lst)))))

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

(defun talkapp/comet-interrupt (entered channel)
  "Produce a list of interrupts."
 (let ((msg-list (talkapp/comet-since-list entered channel))
        (user-list (talkapp/comet-user-state)))
    (cond
      ((and msg-list user-list)
       (append msg-list user-list))
      (msg-list
       msg-list)
      (user-list
       user-list))))

(defun talkapp-comet-handler (httpcon)
  "Defer until there is some change.

A change could be new chat messages, a new user online or offline
or a video call or some other action."
  (with-elnode-auth httpcon 'talkapp-session
    (let* ((username (talkapp-cookie->user-name httpcon))
           (email (aget (db-get username talkapp/user-db) "email"))
           (channel (talkapp/get-channel username))
           (entered (current-time))
           (online (gethash username talkapp/online-cache)))
      ;; Mark the user online with the double hash
      (unless online
        (puthash httpcon email talkapp/httpcon-online-cache)
        (puthash email httpcon talkapp/online-cache))
      ;; Defer till the talk changes
      (elnode-defer-until (talkapp/comet-interrupt entered channel)
          (elnode-send-json httpcon elnode-defer-guard-it :jsonp t)))))

(defun talkapp/comet-fail-hook (httpcon state)
  "Remove the associated user from online state.

Updates `talkapp/user-state-changes' with the username.

Either `closed' or `failed' is the same for this purpose."
  (let ((email (gethash httpcon talkapp/httpcon-online-cache)))
    (when email
      (remhash email talkapp/online-cache)
      (remhash httpcon talkapp/httpcon-online-cache)
      (setq talkapp/user-state-changes
            (append (list (cons email :offline))
                    talkapp/user-state-changes)))))

(defun talkapp-chat-add-handler (httpcon)
  (with-elnode-auth httpcon 'talkapp-session
    (let* ((msg (elnode-http-param httpcon "msg"))
           (username (talkapp-cookie->user-name httpcon))
           (channel (talkapp/get-channel username)))
      (with-current-buffer (get-buffer channel)
        (goto-char (point-max))
        (insert msg)
        (rcirc-send-input))
      (elnode-send-html httpcon "<html>thanks for that chat</html>"))))

(defun talkapp/chat-templater ()
  "Return the list of chats as template."
  (let* ((httpcon elnode-replacements-httpcon)
         (username (talkapp-cookie->user-name httpcon))
         (org ))
    (list
     (cons
      "messages"
      (talkapp/list-to-html username))
     (cons
      "people"
      (talkapp/people-list username)))))

(defun talkapp-chat-handler (httpcon)
  "Handle the chat page."
  ;; FIXME - redirecct when chat not connected?
  (with-elnode-auth httpcon 'talkapp-session
    (elnode-send-file
     httpcon (concat talkapp-dir "chat.html")
     :replacements 'talkapp/chat-templater)))


;; Registration stuff

(defun talkapp-user-handler (httpcon)
  "Present the main user page."
  (with-elnode-auth httpcon 'talkapp-session
    (let ((user-data (talkapp/get-user-http httpcon)))
      (elnode-send-file
       httpcon (concat talkapp-dir "user.html")
       :replacements user-data))))

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
       :cookie-name talkapp-session-cookie-name
       :auth-db talkapp/valid-token-db))))

(defun talkapp/make-user (regform data &optional http-host)
  "Make a user.

Optional HTTP-HOST is the host-name the call was sent to, if
present this is used to try to find an organization for the
user."
  (esxml-form-save
   regform data
   :db-data (lambda (data)
              (destructuring-bind (&key username password key email)
                  (kvalist->plist data)
                ;; Add in the org
                (let ((org (talkapp/get-my-org email http-host)))
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
     :auth-db talkapp/auth-token-db)))

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
      (:db-check "a user with that email exists")))
    (key :html :textarea
         :regex "[A-Za-z0-9-]+"
         :check-failure "just the main part of the key"))
  "Registration form.")

(defun talkapp-registered-handler (httpcon)
  "The registered page.

Makes the validation hash, stores it in the db, queues the email
and directs you to validate."
  (with-elnode-auth httpcon 'talkapp-auth
    (let* ((user (talkapp/get-user httpcon))
           (user-data (talkapp/keyify user))
           (email (aget user-data "email"))
           (username (aget user-data "username"))
           ;; FIXME - this uses elnode's secret key - it would be better to
           ;; use an app specific one
           (email-hash (sha1 (format "%s:%s" elnode-secret-key email))))
      ;; Store the hash with the username and email address
      (db-put email-hash
              `((username . ,username)(email . ,email))
              talkapp/email-valid-db)
      ;; FIXME - We need to send an email!
      (message "talkapp reg %s with verify link %s"
               username
               (format "http://localhost:8101/validate/%s" email-hash))
      ;; Send the file back
      (elnode-send-file
       httpcon (concat talkapp-dir "registered.html")
       :replacements user-data))))

(defun talkapp-register-handler (httpcon)
  "Take a registration and create a user."
  (let ((esxml-field-style :bootstrap))
    (esxml-form-handle
     talkapp-regform httpcon (concat talkapp-dir "register.html")
     (lambda (data)
       (talkapp/save-reg httpcon talkapp-regform data)))))

(defun talkapp-main-handler (httpcon)
  "The handler for the main page."
  (if-elnode-auth httpcon 'talkapp-session
    (elnode-send-file
     httpcon
     (concat talkapp-dir "user.html")
     :replacements 'talkapp/get-user-http)
    ;; Else user is not authenticated so send the main file
    ;;
    ;; FIXME - we should really detect first auth and send registered?
    (elnode-send-file httpcon (concat talkapp-dir "main.html"))))


;;;###autoload
(define-elnode-handler talkapp-router (httpcon)
  "Main router."
  (let ((css (concat talkapp-dir "style.css"))
        (js (concat talkapp-dir "site.js"))
        (md5 (concat talkapp-dir "md5.js"))
        (carousel (concat talkapp-dir "jquery.carouFredSel-6.1.0.js"))
        (jquery (concat talkapp-dir "jquery-1.8.2.min.js"))
        (bootstrap-js (concat talkapp-dir "bootstrap.js"))
        (bootstrap-css (concat talkapp-dir "bootstrap.css")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("^[^/]*//config/" . talkapp-irc-config-handler)
       ("^[^/]*//session/" . talkapp-shoes-off-session)
       ("^[^/]*//chat/" . talkapp-chat-handler)
       ("^[^/]*//send/" . talkapp-chat-add-handler)
       ("^[^/]*//poll/" . talkapp-comet-handler)
       ("^[^/]*//register/" . talkapp-register-handler)
       ("^[^/]*//registered/" . talkapp-registered-handler)
       ("^[^/]*//validate/\\(.*\\)/" . talkapp-validate-handler)
       ("^[^/]*//user/$" . talkapp-user-handler)
       ;; Static content#
       ("^[^/]*//-/style.css" . ,(elnode-make-send-file css))
       ("^[^/]*//-/carousel.js" . ,(elnode-make-send-file carousel))
       ("^[^/]*//-/md5.js" . ,(elnode-make-send-file md5))
       ("^[^/]*//-/site.js" . ,(elnode-make-send-file js))
       ("^[^/]*//-/jquery.js" . ,(elnode-make-send-file jquery))
       ("^[^/]*//-/bootstrap.js" . ,(elnode-make-send-file bootstrap-js))
       ("^[^/]*//-/bootstrap.css" . ,(elnode-make-send-file bootstrap-css))
       ("^[^/]*//$" . talkapp-main-handler)))))


;; First level auth scheme
;;
;; Used after registration to see the email-validation thing.  I guess
;; service notices and such like could be this as well. Also "please
;; turn off any email sending"
(elnode-auth-define-scheme
 'talkapp-auth
 :auth-db talkapp/auth-token-db
 :cookie-name talkapp-cookie-name
 :redirect (elnode-auth-make-login-wrapper
            'talkapp-router))

;; Second level auth scheme
;;
;; Used after email validation.
(elnode-auth-define-scheme
 'talkapp-session
 :auth-db talkapp/valid-token-db
 :cookie-name talkapp-session-cookie-name
 :redirect (elnode-auth-make-login-wrapper
            'talkapp-router))

;;;###autoload
(defun talkapp-start ()
  (interactive)
  (talkapp/rcirc-config)
  (elnode-start 'talkapp-router :port 8100 :host "0.0.0.0")
  (add-hook 'elnode-defer-failure-hook 'talkapp/comet-fail-hook)
  (elnode-deferred-queue-start))

(provide 'talkapp)

;;; talkapp.el ends here
