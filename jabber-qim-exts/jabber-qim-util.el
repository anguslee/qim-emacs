(require 'jabber-qim-env)
(require 'jabber-util)

(defun jabber-qim-muc-jid-p (muc-jid)
  (string= (format "%s.%s" *jabber-qim-muc-sub-hostname*
                   *jabber-qim-hostname*)
           (jabber-jid-server muc-jid)))


;;;###autoload (autoload 'jabber-qim-muc-vcard-group-jid "jabber-qim-extension" "Return group jid" t)
(defun jabber-qim-muc-vcard-group-jid (vcard)
  "Return group jid"
  (decode-coding-string (cdr (assoc 'MN vcard))
                        'utf-8-emacs-unix))

;;;###autoload (autoload 'jabber-qim-muc-vcard-group-display-name "jabber-qim-extension" "Return group display name" t)
(defun jabber-qim-muc-vcard-group-display-name (vcard)
  "Return group display name"
  (decode-coding-string (cdr (assoc 'SN vcard))
                        'utf-8-emacs-unix))

(defun jabber-qim-muc-vcard-group-display-name-update (vcard new-name)
  "Set new display name"
  (ignore-errors
    (setcdr (assoc 'SN
                   vcard)
            new-name)))


;;;###autoload (autoload 'jabber-qim-muc-vcard-group-topic "jabber-qim-extension" "Return group topic" t)
(defun jabber-qim-muc-vcard-group-topic (vcard)
  "Return group topic"
  (ignore-errors
    (decode-coding-string (cdr (assoc 'MT vcard))
                          'utf-8-emacs-unix)))

(defun jabber-qim-muc-vcard-group-topic-update (vcard new-topic)
  "Set group topic"
  (ignore-errors
    (setcdr (assoc 'MT
                   vcard)
            new-topic)))

;;;###autoload (autoload 'jabber-qim-user-vcard-jid "jabber-qim-extension" "Return user jid" t)
(defun jabber-qim-user-vcard-jid (vcard)
  "Return user jid"
  (format "%s@%s" (decode-coding-string (cdr (assoc 'U vcard))
                                        'utf-8-emacs-unix)
          *jabber-qim-hostname*))

;;;###autoload (autoload 'jabber-qim-user-vcard-name "jabber-qim-extension" "Return user name" t)
(defun jabber-qim-user-vcard-name (vcard)
  "Return user name"
  (decode-coding-string (cdr (assoc 'N vcard))
                        'utf-8-emacs-unix))

;;;###autoload (autoload 'jabber-qim-user-vcard-position "jabber-qim-extension" "Return user position" t)
(defun jabber-qim-user-vcard-position (vcard)
  "Return user position"
  (decode-coding-string (cdr (assoc 'D vcard))
                        'utf-8-emacs-unix))

(defun jabber-qim-user-jid-by-completion (completion)
  (if (assoc-string completion
                    *jabber-qim-username-to-jid-cache*)
      (cdr (assoc-string completion
                         *jabber-qim-username-to-jid-cache*))
    completion))

(defun jabber-qim-user-jid-completion-list ()
  (append (mapcar #'car
                  *jabber-qim-username-to-jid-cache*)
          *jabber-qim-user-jid-cache*))

(defun jabber-qim-session-user-vcards ()
  (let ((ret '()))
    (maphash #'(lambda (key value)
                 (add-to-list 'ret value))
             *jabber-qim-user-vcard-cache*)
    ret))


;;;###autoload (autoload 'jabber-qim-jid-nickname "jabber-qim-extension" "Return user nickname" t)
(defun jabber-qim-jid-nickname (jid)
  "Return user nickname"
  (when jid
    (let ((user-vcard (gethash (jabber-jid-user jid)
                               *jabber-qim-user-vcard-cache*)))
      (when user-vcard
        (jabber-qim-user-vcard-name user-vcard)))))

(defun jabber-qim-readmark-message-p (message)
  (string= "readmark" (jabber-xml-get-attribute message 'type)))

(defun secure-hash-file (file algorithm)
  (when (file-exists-p file)
    (let ((coding-system-for-read 'no-conversion))
      (with-temp-buffer
        (insert-file-contents file)
        (secure-hash algorithm (current-buffer))))))


(provide 'jabber-qim-util)
