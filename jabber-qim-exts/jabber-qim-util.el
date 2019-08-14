;; jabber-qim-util.el QTalk extension common util procedures.

;; Copyright (C) 2018 - Angus Lee - angus.lee8329@gmail.com

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'jabber-qim-env)
(require 'jabber-util)

(defun jabber-qim-muc-jid-p (muc-jid)
  ;; (string= (format "%s.%s" *jabber-qim-muc-sub-hostname*
  ;;                  *jabber-qim-domain*)
  ;;          (jabber-jid-server muc-jid))
  (string-prefix-p (format "%s." *jabber-qim-muc-sub-hostname*)
                   (jabber-jid-server muc-jid)))


;;;###autoload (autoload 'jabber-qim-muc-vcard-group-jid "jabber-qim-extension" "Return group jid" t)
(defun jabber-qim-muc-vcard-group-jid (vcard)
  "Return group jid"
  (ignore-errors
    (decode-coding-string (cdr (assoc 'MN vcard))
                          'utf-8-emacs-unix)))

;;;###autoload (autoload 'jabber-qim-muc-vcard-group-display-name "jabber-qim-extension" "Return group display name" t)
(defun jabber-qim-muc-vcard-group-display-name (vcard)
  "Return group display name"
  (let ((sn-val (ignore-errors
                  (decode-coding-string (cdr (assoc 'SN vcard))
                                       'utf-8-emacs-unix))))
    (if (> (length sn-val) 0)
        sn-val
      (jabber-qim-muc-vcard-group-jid vcard))))

(defun jabber-qim-muc-vcard-group-display-name-update (vcard new-name)
  "Set new display name"
  (ignore-errors
    (setcdr (assoc 'SN
                   vcard)
            new-name)))

(defun jabber-qim-jid-domain (jid)
  (if (jabber-qim-muc-jid-p jid)
      (cadr (split-string (jabber-jid-server jid)
                          "[.]" t))
    (jabber-jid-server jid)))


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
  (and (cdr (assoc 'U vcard))
       (format "%s@%s" (decode-coding-string (cdr (assoc 'U vcard))
                                             'utf-8-emacs-unix)
               *jabber-qim-domain*)))

;;;###autoload (autoload 'jabber-qim-user-vcard-name "jabber-qim-extension" "Return user name" t)
(defun jabber-qim-user-vcard-name (vcard)
  "Return user name"
  (and (cdr (assoc 'N vcard))
       (decode-coding-string (cdr (assoc 'N vcard))
                             'utf-8-emacs-unix)))

;;;###autoload (autoload 'jabber-qim-user-vcard-position "jabber-qim-extension" "Return user position" t)
(defun jabber-qim-user-vcard-position (vcard)
  "Return user position"
  (and (cdr (assoc 'D vcard))
       (decode-coding-string (cdr (assoc 'D vcard))
                             'utf-8-emacs-unix)))

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

(defun jabber-qim-control-message-p (message)
  (or
   (string= "readmark" (jabber-xml-get-attribute message 'type))
   (string= "mstat" (jabber-xml-get-attribute message 'type))
   (string= "revoke" (jabber-xml-get-attribute message 'type))))

(defun jabber-qim-revoke-message-p (message)
  (string= "revoke" (jabber-xml-get-attribute message 'type)))

(defun jabber-qim-message-extend-info (xml-data)
  (ignore-errors
    (let ((extend-info-node (jabber-xml-get-attribute
                             (car (jabber-xml-get-children
                                   xml-data 'body))
                             'extendInfo)))
      (when extend-info-node
        (cl-remove-if-not
         #'(lambda (x)
             (and
              (sequencep (cdr x))
              (> (length (cdr x))
                 0)
              (find (car x)
                    (list 'title 'desc 'linkurl))))
         (json-read-from-string extend-info-node))))))

(defun jabber-qim-message-body-text (xml-data)
  (let ((msg-type (jabber-qim-message-type xml-data))
        (extend-info (jabber-qim-message-extend-info xml-data)))
    (if (string-equal msg-type jabber-qim-msg-type-common-trd-info)
        (let ((desc (cdr (assoc 'desc extend-info)))
              (title-and-link (format "%s %s"
                                      (or (cdr (assoc 'title extend-info))
                                          "Link:")
                                      (cdr (assoc 'linkurl extend-info)))))
          (if desc
              (format "%s\n%s" title-and-link desc)
            title-and-link))
      (car
       (jabber-xml-node-children
        (car
         (jabber-xml-get-children xml-data 'body)))))))


(defun secure-hash-file (file algorithm)
  (when (file-exists-p file)
    (let ((coding-system-for-read 'no-conversion))
      (with-temp-buffer
        (insert-file-contents file)
        (secure-hash algorithm (current-buffer))))))

(defun jabber-qim-api-connection-auth-info (jc)
  (let ((connection-state (plist-get jc
                                     :state-data)))
    `((:u . ,(plist-get connection-state
                        :username))
      (:k . ,(plist-get connection-state
                        :qim-auth-key))
      (:d . ,(plist-get connection-state
                        :server))
      (:t . ,(truncate (float-time))))))

(provide 'jabber-qim-util)
