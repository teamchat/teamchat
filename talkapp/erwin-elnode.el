;;; erwin functions in elnode -*- lexical-binding: t -*-

(require 'elnode)

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

(defun erwin-elnode-router (httpcon)
  "Top level handler dispatching requests for Erwin stuff."
  (elnode-hostpath-dispatcher
   httpcon
   '(("^[^/]+//insult/" . erwin-elnode/insult-handler))))

;;; erwin-elnode.el ends here
