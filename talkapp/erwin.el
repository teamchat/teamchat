;;; erwin

;; Example printhooks call
;; (irc.teamchat.net~njferrier) [nferrier] {PRIVMSG} [#thoughtworks] /testy!/

(require 'rcirc)

(defvar erwin/db '()
  "Erwin's alist database of regexs to functions.")

(defun erwin/db-put (regex handler)
  (unless (aget erwin/db regex)
    (setq erwin/db (acons regex handler erwin/db))))

(defun erwin/detect (text)
  "Detect erwin in the TEXT."
  (when (string-match
         "\\(^erwin[:, ]\\|[ ,;]erwin[.?, ;]\\|[ ,;]erwin$\\)"
         text) text))

(defun erwin/send (process channel data)
  "Send DATA to CHANNEL on PROCESS."
  (with-current-buffer (rcirc-get-buffer process channel)
    (save-excursion
      (goto-char (point-max))
      (insert data)
      (rcirc-send-input))))

(defun erwin-input (process sender target text)
  "`rcirc-print-hooks' handler to connect erwin."
  (when (or
         (equal target "erwin")
         ;; we could have additional text matches here like: ???
         (erwin/detect text))
    ;; Do erwin stuff on text
    (let ((fn (loop for (regex . handler) in erwin/db
                 if (string-match regex text) return handler)))
      (message "the fn is %S" fn)
      (funcall fn process sender target text))))


;; Some simple built in robots

(defvar erwin-insult-adjectives-list
  (list
   "stinky"
   "tiny-minded"
   "pea-brained"
   "heavily lidded"
   "muck minded"
   "flat footed")
  "List of adjectives used in the insulter.")

(defvar erwin-insult-nouns-list
  (list
   "bog warbler"
   "tin pincher"
   "yeti"
   "whoo-har")
  "List of nouns used in the insulter.")

(defun erwin-insult (process sender target text)
  "Insult people."
  (when (string-match "^erwin[:, ] *insult \\(.*\\)" text)
    (let ((name (match-string 1 text))
          (gielgud-p (equal 10 (random 11)))
          (adjective
           (elt
            erwin-insult-adjectives-list
            (random (length erwin-insult-adjectives-list))))
          (noun
           (elt
            erwin-insult-nouns-list
            (random (length erwin-insult-nouns-list)))))
      (if gielgud-p
          (erwin/send
           process target
           (format "%s: wash your own arse you little shit." sender))
          ;; Else respond with the insult
          (erwin/send
           process target
           (format "%s is a %s %s." name adjective noun))))))

(erwin/db-put "insult" 'erwin-insult)

(defun erwin-hammertime (process sender target text)
  "Impersonate MC Hammer."
  (let ((quotes (list
                 "READY THE ENORMOUS TROUSERS!"
                 "YOU CAN'T TOUCH THIS!")))
    (erwin/send process target (elt quotes (random (length quotes))))))

(erwin/db-put "hammertime" 'erwin-hammertime)


(provide 'erwin)

;;; erwin.el ends here
