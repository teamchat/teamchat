;;; talk.el --- make talking easy

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: comm
;; Version: 0.0.1
;; Package-Requires: ((elnode "0.9.9.6.2")(anaphora "0.0.5")(esxml "0.0.1")(db "0.0.1")(uuid "0.0.3"))

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

(elnode-app talk-dir anaphora esxml esxml-form db kv uuid)

(defconst talk--user-db
  (db-make
   `(db-hash
     :filename
     ,(expand-file-name
       (concat talk-dir "auth-db"))))
  "The database where we store authentication details.")

(defun talk--db-filter-get (key value)
  (aget value "token"))

(defconst talk--auth-token-db
  (db-make
   `(db-filter
     :source ,talk--user-db
     :filter
     talk--db-filter-get))
  "Token view of the user db.")

(defconst talk-cookie-name "talk-user"
  "The name of the cookie we use for auth.")


(defvar talk--irc-server-name "irc.jbx.cc")
(defvar talk--irc-server-port 6667)
(defvar talk--irc-channels '("#thoughtworks"))

(defun talk--irc-details (username password email)
  (list :username username :password password
        :server-alist
        `((,talk--irc-server-name
           :nick ,username
           :port ,talk--irc-server-port
           :user-name ,username
           :password "secret"
           :full-name ,email
           :channels ,talk--irc-channels))))

(defun talk-list-ssh-keys (&optional make-buf)
  "Make an alist of the ssh-keys from the user database."
  (interactive (list t))
  (let ((keys (db-map
               (lambda (key user-data)
                 (when user-data
                   (kvdotassoc 'key user-data)))
               talk--user-db)))
    (if make-buf
        (with-current-buffer (get-buffer-create "*talk-keys*")
          (setq buffer-read-only t)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (goto-char (point-min))
            (loop for key in keys
               do (insert (format "%s -- %s\n" (cdr key) (car key))))
            (switch-to-buffer (current-buffer))))
        keys)))


;; Retrieval bits

(defun talk-get-user (&optional httpcon)
  "Get the user via the cookie on the HTTPCON."
  (db-get
   (car
    (elnode-auth-cookie-decode
     (elnode-http-cookie
      (if httpcon httpcon elnode-replacements-httpcon)
      swarmweb-cookie-name t)))
   swarmweb--user-db))

(defun* talk--login (httpcon
                        registered-page
                        &key
                        username
                        password)
  "Log the registered user in."
  (elnode-auth-http-login
   httpcon
   username password
   registered-page
   :cookie-name talk-cookie-name
   :auth-db talk--auth-token-db))

(defun talk--make-user (regform data)
  "Make a user."
  (esxml-form-save
   regform data
   :db-data (lambda (data)
              (destructuring-bind (&key username password key email)
                  (kvalist->plist data)
                (let* ((token (elnode--auth-make-hash username password)))
                  (acons "token" token data))))))

(defun talk--get-shoes-off-config (username)
  "Return a db record in the form that can be used by shoes-off."
  (awhen (db-get username talk--user-db)
    (destructuring-bind (&key token username password key email)
        (kvalist->plist it)
      (talk--irc-details username password email))))

(defun talk--shoes-off-auth (username-spec password)
  "DB based implementation of `shoes-off--auth-check'.

We should expect USERNAME-SPEC to just be a username."
  (let* ((record (db-query talk--user-db `(= "username" ,username-spec)))
         (details (aget record username-spec)))
    (destructuring-bind (&key username password email key token)
        (kvalist->plist details)
      (when (equal password (aget details "password"))
        (talk--irc-details username password email)))))

(defun talk--save-reg (httpcon regform data)
  (let ((user (talk--make-user regform data)))
    (talk--login
     httpcon "/registered/"
     :username (aget user "username")
     :password (aget user "password"))))

(defconst talk-regform
  (esxml-form
    (:db talk--user-db :db-key "username")
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

(defun talk-register-handler (httpcon)
  "Take a registration and create a user."
  (esxml-form-handle
   talk-regform httpcon (concat talk-dir "register.html")
   (lambda (data)
     (talk--save-reg httpcon talk-regform data))))

(defun talk-main-handler (httpcon)
  "The handler for the main page."
  (if-elnode-auth httpcon 'talk-auth
    (elnode-send-file httpcon (concat talk-dir "registered.html"))
    ;; Else user is not authenticated so send the main file
    (elnode-send-file httpcon (concat talk-dir "main.html"))))

(define-elnode-handler talk-router (httpcon)
  (let ((registered (concat talk-dir "registered.html"))
        (css (concat swarmweb-dir "style.css"))
        (js (concat swarmweb-dir "site.js")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("^[^/]*//register/" . talk-register-handler)
       ("^[^/]*//registered/"
        . ,(elnode-make-send-file
            registered
            :replacements 'talk--get-user-http))
       ("^[^/]*//style.css" . ,(elnode-make-send-file css))
       ("^[^/]*//site.js" . ,(elnode-make-send-file js))
       ("^[^/]*//$" . swarmweb-main-handler)))))

(elnode-auth-define-scheme
 'talk-auth
 :auth-db talk--auth-token-db
 :cookie-name talk-cookie-name
 :redirect (elnode-auth-make-login-wrapper
            'talk-router))


(provide 'talk)

;;; talk.el ends here
