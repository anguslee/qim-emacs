;; Util functions for qim

(require 'json)
(require 'web)
(require 'latch)
(require 'jabber-util)

(add-to-list 'web-json-expected-mimetypes-list
             "text/json")

(defvar jabber-qim-api-server
  "https://qtapi.corp.qunar.com")


;;;###autoload (autoload 'jabber-qim-muc-vcard-group-jid "jabber-qim-util" "Return group jid" t)
(defun jabber-qim-muc-vcard-group-jid (vcard)
  "Return group jid"
  (decode-coding-string (cdr (assoc 'MN vcard))
                        'utf-8-emacs-unix))

;;;###autoload (autoload 'jabber-qim-muc-vcard-group-display-name "jabber-qim-util" "Return group display name" t)
(defun jabber-qim-muc-vcard-group-display-name (vcard)
  "Return group display name"
  (decode-coding-string (cdr (assoc 'SN vcard))
                        'utf-8-emacs-unix))

;;;###autoload (autoload 'jabber-qim-muc-vcard-group-topic "jabber-qim-util" "Return group topic" t)
(defun jabber-qim-muc-vcard-group-topic (vcard)
  "Return group topic"
  (decode-coding-string (cdr (assoc 'MT vcard))
                        'utf-8-emacs-unix))


(defun jabber-qim-api-request-post (callback command data mime-type)
  (setq *jabber-qim-api-response* nil)
  (web-json-post 
   callback
   :url (format "%s/%s" jabber-qim-api-server command)
   :data data
   :mime-type mime-type
   :json-object-type 'alist
   :json-array-type 'list))

;;;###autoload
(defvar *jabber-qim-muc-vcard-cache*
  (make-hash-table :test 'equal))

;;;###autoload (autoload 'jabber-qim-get-muc-vcard "jabber-qim-util" "Return MUC vcard" t)
(defun jabber-qim-get-muc-vcard (muc-jid)
  "Return MUC vcard"
  (and
   (string-prefix-p "conference." (jabber-jid-server muc-jid))
   (or (gethash muc-jid *jabber-qim-muc-vcard-cache*)
       (lexical-let ((latch (make-one-time-latch))
                     (ret nil))
         (jabber-qim-api-request-post
          (lambda (data conn headers)
            (setq ret
                  (nth 0 (cdr (assoc 'data data))))
            (apply-partially #'nofify latch))
          "getmucvcard"
          (json-encode (vector `((:muc_name . ,muc-jid)
                                 (:version . 0))))
          'applicaition/json)
         (wait latch 0.5)
         (puthash muc-jid ret *jabber-qim-muc-vcard-cache*)
         ret))))

(provide 'jabber-qim-util)

