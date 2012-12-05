;;; erwin - Emacs Robots Within IRC Network

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

(defun erwin-insult (process sender target text)
  "Insult people."
  (when (string-match "^erwin[:, ] *insult \\(.*\\)" text)
    (let ((name (match-string 1 text)))
      (web-json-post
       (lambda (data httpcon header)
         (erwin/send process target (aget data 'insult)))
       :url "http://localhost:8007/insult/"
       :data (list (cons "who" name)
                   (cons "sender" sender)
                   (cons "target" target))))))

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
