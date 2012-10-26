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

(defconst talkapp/user-db
  (db-make
   `(db-hash
     :filename
     ,(expand-file-name
       (concat talkapp-dir "auth-db"))))
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
       (concat talkapp-dir "email-db"))))
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


;; Auth cookie constants

(defconst talkapp-session-cookie-name "talkapp-session"
  "The name of the cookie we use for email-valid auth.")

(defconst talkapp-cookie-name "talkapp-user"
  "The name of the cookie we use for auth.")

;; IRC setup stuff

(defvar talkapp/irc-server-name "irc.jbx.cc")
(defvar talkapp/irc-server-port 6667)
(defvar talkapp/irc-channels '("#thoughtworks"))

(defun talkapp/irc-details (username password email)
  (list :username username :password password
        :server-alist
        `((,talkapp/irc-server-name
           :nick ,username
           :port ,talkapp/irc-server-port
           :user-name ,username
           :password "secret"
           :full-name ,email
           :channels ,talkapp/irc-channels))))

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
        (kvalist->plist details)
      (when (equal password (aget details "password"))
        (talkapp/irc-details username password email)))))


;; Only do the rcirc init if we need to.
;; If this variable is not bound or bound and t it will eval.
(when (or (not (boundp 'talkapp-do-rcirc)) talkapp-do-rcirc)
  (eval-after-load "talkapp"
    '(progn
      (setq shoes-off--get-config-plugin 'talkapp/get-shoes-off-config)
      (setq shoes-off--auth-plugin 'talkapp/shoes-off-auth)
      (setq shoes-off--rcirc-connect-plugin 'talkapp-rcirc-connect)
      ;; Ensures the time format support us pulling back accurately
      (setq rcirc-time-format "%Y-%m-%d %H:%M:%S:%N "))))


;; Retrieval bits

(defun talkapp-cookie->user-name (httpcon)
  "Convert the cookie to a user-name."
  (car
   (elnode-auth-cookie-decode
    (elnode-http-cookie
     (if httpcon httpcon elnode-replacements-httpcon)
     talkapp-cookie-name t))))

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
      (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
      (if (or (not (boundp 'talkapp-do-rcirc)) talkapp-do-rcirc)
          (elnode-child-process
           httpcon
           "bash" "/home/emacs/ircdmakeuser" username)
          (elnode-send-json httpcon (list :error t))))))

;; Start the shoes-off session for this user

(defun talkapp-shoes-off-session (httpcon)
  "Manage sessions from the webapp."
  (with-elnode-auth httpcon 'talkapp-session
    (let* ((username (talkapp-cookie->user-name httpcon))
           ;; FIXME hard coded server for now
           (session (gethash
                     (concat username "@" talkapp/irc-server-name)
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
          (forward-line -1)
          (re-search-backward irc-line-re nil t)
          (setq doit
                (time-less-p
                 time-to-find
                 (talkapp/timestamp->time
                  (condition-case nil
                      (match-string-no-properties 1)
                    (error "1970-01-01 00:00:00:000000"))))))
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

(defun talkapp/entry->html (username message)
  "Return the templated form of the row."
  `(tr
    ()
    (td
     ((class . ,(concat "username " username)))
     ,username)
    (td
     ((class . "message"))
     ,message)))

(defconst talkapp/default-chat-history-minutes 60)

(defun talkapp/chat-list (channel)
  "Make a list of the CHANNEL chatter."
  (talkapp/list-since-mins-ago
   talkapp/default-chat-history-minutes
   channel))

(defun talkapp/chat-list-test (channel)
  "Replacement for `talkapp/chat-list' that makes dummy chat."
  '(("2012-10-24 08:23:00" "nic" "this is a test")
    ("2012-10-24 08:23:00" "jim" "a conversation could occur")))

(defun talkapp/list-to-html (username)
  "Return the list of chat as rows for initial chat display."
  (let ((channel (concat "#thoughtworks@localhost~" username)))
    (loop for entry in (talkapp/chat-list channel)
       if (equal 3 (length entry))
       concat
         (esxml-to-xml
          (talkapp/entry->html (elt entry 1)(elt entry 2))))))

(defun talkapp/chat-templater ()
  "Return the list of chats as template."
  (let* ((httpcon elnode-replacements-httpcon)
         (username (talkapp-cookie->user-name httpcon)))
    (list
     (cons
      "messages"
      (talkapp/list-to-html username)))))

(defun talkapp-chat-handler (httpcon)
  "Handle the chat page."
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

(defun talkapp/make-user (regform data)
  "Make a user."
  (esxml-form-save
   regform data
   :db-data (lambda (data)
              (destructuring-bind (&key username password key email)
                  (kvalist->plist data)
                (let* ((token (elnode--auth-make-hash username password)))
                  (acons "token" token data))))))

(defun talkapp/save-reg (httpcon regform data)
  (let* ((user (talkapp/make-user regform data))
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
     :replacements user-data)))

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
  (let (
        (css (concat talkapp-dir "style.css"))
        (js (concat talkapp-dir "site.js"))
        (jquery (concat talkapp-dir "jquery-1.8.2.min.js"))
        (bootstrap-js (concat talkapp-dir "bootstrap.js"))
        (bootstrap-css (concat talkapp-dir "bootstrap.css")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("^[^/]*//config/" . talkapp-irc-config-handler)
       ("^[^/]*//session/" . talkapp-shoes-off-session)
       ("^[^/]*//chat/" . talkapp-chat-handler)
       ("^[^/]*//register/" . talkapp-register-handler)
       ("^[^/]*//registered/" . talkapp-registered-handler)
       ("^[^/]*//validate/\\(.*\\)/" . talkapp-validate-handler)
       ("^[^/]*//user/$" . talkapp-user-handler)
       ;; Static content
       ("^[^/]*//style.css" . ,(elnode-make-send-file css))
       ("^[^/]*//site.js" . ,(elnode-make-send-file js))
       ("^[^/]*//jquery.js" . ,(elnode-make-send-file jquery))
       ("^[^/]*//bootstrap.js" . ,(elnode-make-send-file bootstrap-js))
       ("^[^/]*//bootstrap.css" . ,(elnode-make-send-file bootstrap-css))
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

(provide 'talkapp)

;;; talkapp.el ends here
