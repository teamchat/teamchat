;; extra utils for talkapp

(defun* talkapp-utils/make-a-user (username email &key password sshkey)
  "Comprehensive user creation tool."
  (let* ((pword (or
                 password
                 (substring
                  (base64-encode-string
                   (number-to-string (random)))
                  0 10)))
         (user (talkapp/make-user
                talkapp-regform
                (list (cons "username" username)
                      (cons "password" pword)
                      (cons "email" email)))))
    ;; Force the valid
    (db-put
     username
     (acons "valid" t
            (db-get username talkapp/user-db)) talkapp/user-db)
    ;; Update the key
    (when sshkey
      (talkapp/key-save username "default" sshkey))))

(defun talkapp-utils/make-validation-links (user-query &optional valid)
  "Make an alist of validation links.

USER-QUERY specifies a query to find users, for example:

  '(~ \"email\" \".*example.*\"))

will produce validation links for any user with an email matching
that regex.

If VALID is `t' then no validation check is done, otherwise only
non valid users will be examined."
  (let (email-rec)
    (loop for (user . user-rec)
       in (db-query
           talkapp/user-db
           (if valid
               user-query
               `(&(= 'valid nil) ,user-query)))
       do
         (setq email-rec
               (db-query
                talkapp/email-valid-db
                `(= "username" ,user)))
       if email-rec
       if (db-get (aget user-rec "org") talkapp/org-db)
       collect
         (let* ((org-rec (db-get (aget user-rec "org") talkapp/org-db))
                (host (aget org-rec "host"))
                (validkey (caar email-rec))
                (record (cdar email-rec))
                (email (aget record 'email)))
           (list email
                 (format
                  "http://%s/validate/%s/"
                  host validkey))))))

;; talkapp-utils.el ends
