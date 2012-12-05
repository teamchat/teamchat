;;; erwin functions in elnode -*- lexical-binding: t -*-

(elnode-app erwin-elnode-dir
    db)

(defvar erwin-insult-adjectives-list
  (list "ill-conceived"
        "delusional"
        "disproven"
        "poorly-researched"
        "needhamish"
        "so-called"
        "jumped-up"
        "touch-typing"
        "randian"
        "tree-hugging"
        "bleeding-heart"
        "elnode "
        "bassoonist "
        "oboe-playing"
        "big fat"
        "freedom-hating"
        "functional gab"
        "excel-loving"
        "post-technical"
        "dirigible-shaped"
        "enterprisey"
        "ruby threaded"
        "hipster"
        "tinsel cheeked"
        "stinky"
        "tiny-minded"
        "pea-brained"
        "heavily lidded"
        "muck minded"
        "flat footed")
  "List of adjectives used in the insulter.")

(defvar erwin-insult-nouns-list
  (list
   "decepticon"
   "kief"
   "suit"
   "luser"
   "sharepoint-developer"
   "liberal"
   "socialist "
   "Tutankhamun"
   "gimboid"
   "person who thinks digital watches are a really neat idea"
   "smeghead"
   "jobbie-face"
   "derpgineer"
   "urban dictionary contributor"
   "manager"
   "power point jockey"
   "windows user"
   "brogrammer"
   "long winded fun bus hater"
   "hipster"
   "windy nonsense talker"
   "idiot who can"
   "category theory dumbass"
   "smeggy pants"
   "java programmer"
   "bog warbler"
   "tin pincher"
   "yeti"
   "whoo-har")
  "List of nouns used in the insulter.")

(defun erwin-elnode/insult-handler (httpcon)
  "Insult someone specified.

Every now and then just randomly yell stuff unrelated to the
requested insult.

A bit of silly fun."
  (elnode-method httpcon
    (POST
     (let* ((whom (elnode-http-param httpcon "who"))
            (sender (elnode-http-param httpcon "sender"))
            (target (elnode-http-param httpcon "target"))
            (gielgud-p (equal 10 (random 11)))
            (bunny-p (equal 10 (random 11)))
            (adjective
             (elt
              erwin-insult-adjectives-list
              (random (length erwin-insult-adjectives-list))))
            (noun
             (elt
              erwin-insult-nouns-list
              (random (length erwin-insult-nouns-list)))))
       (elnode-send-json
        httpcon
        (list
         :insult
         (cond
           (gielgud-p
            (format "%s: wash your own arse you little shit." sender))
           (bunny-p
            (format "NOBODY FUCKING MOVE! THIS IS A ROBBERY!"))
           ;; Else respond with the proper insult
           (t
            (format "%s is a %s %s." whom adjective noun)))))))))

(defconst erwin-elnode/cred-db
  (db-make
   `(db-hash
     :filename
     ,(expand-file-name
       (concat
        (file-name-as-directory erwin-elnode-dir)
        "cred-db"))))
  "Database of cred.

Key is the username of the person with cred.

The fields in the record are: last-giver, count, last-time.")

(defun erwin-elnode/cred-handler (httpcon)
  "A real cred bot.

Stores cred for everyone in a database."
  (elnode-method httpcon
    (POST
     (let* ((whom (elnode-http-param httpcon "who"))
            (sender (elnode-http-param httpcon "sender"))
            (target (elnode-http-param httpcon "target"))
            (current-cred (db-get whom erwin-elnode/cred-db)))
       (if (or
            ;; someone trying to game it on behalf of someone else
            (and
             current-cred
             (equal (aget current-cred "last-giver") sender))
            ;; someone trying to give themselves cred
            (equal whom sender))
           (elnode-send-json httpcon '(:cred "can't game it that way"))
           ;; Else it's good - update the record
           (let ((count (if current-cred
                            (+ (aget current-cred "count") 1)
                            1)))
             (db-put
              whom
              (list (cons "last-giver" sender)
                    (cons "last-time" (current-time))
                    (cons "count" count)) erwin-elnode/cred-db)
             ;; Now send the json response
             (elnode-send-json
              httpcon
              (list :cred (format "one point for %s" whom)
                    :owner whom
                    :count count))))))))

(defun erwin-elnode-router (httpcon)
  "Top level handler dispatching requests for Erwin stuff."
  (elnode-hostpath-dispatcher
   httpcon
   '(("^[^/]+//insult/" . erwin-elnode/insult-handler)
     ("^[^/]+//cred/" . erwin-elnode/cred-handler))))

;;; erwin-elnode.el ends here
