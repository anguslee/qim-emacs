;;; Extensions for qim -*- lexical-binding: t -*-

(require 'json)
(require 'web)
(require 'latch)
(require 'jabber-util)
(require 'jabber-avatar)
(require 'subr-x)


(add-to-list 'web-json-expected-mimetypes-list
             "text/json")

(defvar *jabber-qim-api-server*
  "https://qtapi.corp.qunar.com")

(defvar *jabber-qim-file-server*
  "https://qtalk.corp.qunar.com")

(defvar jabber-qim-local-file-dir
  (format "%s.cache"
          (file-name-directory
           (or load-file-name buffer-file-name))))

(defvar *jabber-qim-hostname*
  "ejabhost1")

(defvar *jabber-qim-muc-sub-hostname*
  "conference")


(defun jabber-qim-local-images-cache-dir ()
  (format "%s/images" jabber-qim-local-file-dir))

(defun jabber-qim-local-received-files-cache-dir ()
  (format "%s/received-files" jabber-qim-local-file-dir))

(ignore-errors
  (dired-create-directory jabber-qim-local-file-dir))

(ignore-errors
  (dired-create-directory
   (jabber-qim-local-images-cache-dir)))

(ignore-errors
  (dired-create-directory
   (jabber-qim-local-received-files-cache-dir)))


(defvar jabber-qim-local-emotions-directory
  (format "%s%s"
          (file-name-directory
           (or load-file-name buffer-file-name))
          "resources/Emotions"))

(defun jabber-qim-muc-jid-p (muc-jid)
  (string= (format "%s.%s" *jabber-qim-muc-sub-hostname*
                   *jabber-qim-hostname*)
           (jabber-jid-server muc-jid)))


(defun jabber-qim-api-request-post (callback command data mime-type)
  (web-json-post 
   callback
   :url (format "%s/%s" *jabber-qim-api-server* command)
   :data data
   :mime-type mime-type
   :json-object-type 'alist
   :json-array-type 'list))


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

;;;###autoload (autoload 'jabber-qim-muc-vcard-group-topic "jabber-qim-extension" "Return group topic" t)
(defun jabber-qim-muc-vcard-group-topic (vcard)
  "Return group topic"
  (decode-coding-string (cdr (assoc 'MT vcard))
                        'utf-8-emacs-unix))

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


;;;###autoload
(defvar *jabber-qim-user-vcard-cache*
  (make-hash-table :test 'equal))

;;;###autoload
(defvar *jabber-qim-user-jid-cache*
  '())

(defun jabber-qim-session-user-vcards ()
  (let ((ret '()))
    (maphash #'(lambda (key value)
                 (add-to-list 'ret value))
             *jabber-qim-user-vcard-cache*)
    ret))


(jabber-qim-api-request-post
 #'(lambda (data conn headers)
     (mapcar #'(lambda (vcard)
                 (add-to-list '*jabber-qim-user-jid-cache*
                              (jabber-jid-symbol (jabber-qim-user-vcard-jid vcard)))
                 (puthash (jabber-qim-user-vcard-jid vcard)
                          vcard *jabber-qim-user-vcard-cache*)) data))
 "getusers"
 "u="
 'applicaion/json)

;;;###autoload (autoload 'jabber-qim-jid-nickname "jabber-qim-extension" "Return user nickname" t)
(defun jabber-qim-jid-nickname (jid)
  "Return user nickname"
  (let ((user-vcard (gethash (jabber-jid-user jid)
                             *jabber-qim-user-vcard-cache*)))
    (when user-vcard
      (jabber-qim-user-vcard-name user-vcard))))


;;;###autoload
(defvar *jabber-qim-muc-vcard-cache*
  (make-hash-table :test 'equal))

(defvar *jabber-qim-muc-initial-members*
  (make-hash-table :test 'equal))


(defun jabber-qim-session-muc-vcards ()
  (let ((ret '()))
    (maphash #'(lambda (key value)
                 (add-to-list 'ret value))
             *jabber-qim-muc-vcard-cache*)
    ret))

(defun jabber-qim-session-muc-vcard-alist ()
  (mapcar #'(lambda (vcard)
              (cons (intern (jabber-qim-muc-vcard-group-display-name vcard))
                    (jabber-qim-muc-vcard-group-jid vcard)))
          (jabber-qim-session-muc-vcards)))


;; (defun jabber-qim-set-muc-vcard (muc-jid nickname title desc pic-url)
;;   (json-encode (vector `((:muc_name . ,muc-jid)
;;                          (:nick . ,nickname)
;;                          (:title . ,title)
;;                          (:desc . ,desc)
;;                          (:version . 0)))))

;; (jabber-qim-api-request-post
;;  (lambda (data conn headers)
;;    (message "%s" headers)
;;    (message "%s" data))
;;  "setmucvcard"
;;  (jabber-qim-set-muc-vcard "test22323-angus@conference.ejabhost1" "hahaha" "ddd" "desc" nil)
;;  'application/json)

;;;###autoload
(defcustom jabber-qim-muc-autojoin nil
  "List of QIM MUC rooms to automatically join on connection.
This list is saved in your Emacs customizations.  You can also store
such a list on the Jabber server, where it is available to every
client; see `jabber-edit-bookmarks'."
  :group 'jabber-chat
  :type '(repeat (string :tag "JID of QIM chatroom")))

(defun jabber-qim-muc-autojoin (jc)
  "Join rooms specified in account bookmarks and global `jabber-muc-autojoin'."
  (interactive (list (jabber-read-account)))
  (when (bound-and-true-p jabber-qim-muc-autojoin)
    (dolist (group jabber-qim-muc-autojoin)
      (let ((muc-jid (car group))
            (muc-properties (cdr group)))
        (jabber-qim-muc-join jc muc-jid)
        (when (cdr (assoc :silence muc-properties))
          (unless (find muc-jid *jabber-silenced-groupchats* :test 'equal)
            (add-to-list '*jabber-silenced-groupchats*
                         muc-jid)))))))

(add-to-list 'jabber-post-connect-hooks 'jabber-qim-muc-autojoin)


;;;###autoload (autoload 'jabber-qim-muc-join "jabber-qim-extension" "Join a qim MUC chatroom" t)
(cl-defun jabber-qim-muc-join (jc muc-jid &optional popup)
  "Join a qim MUC chatroom"
  (interactive
   (list (jabber-read-account)
         (let ((muc-name
                (jabber-read-jid-completing "group: "
                                            (mapcar #'car
                                                    (-filter #'(lambda (muc)
                                                                 (not (find (cdr muc)
                                                                            (mapcar #'car *jabber-active-groupchats*)
                                                                            :test 'string=)))
                                                             *jabber-qim-user-muc-room-jid-list*)))))
           (if (assoc-string muc-name
                             *jabber-qim-user-muc-room-jid-list*)
               (cdr (assoc-string muc-name
                                  *jabber-qim-user-muc-room-jid-list*))
             muc-name))
         t))
  (if (string-prefix-p (format "%s." *jabber-qim-muc-sub-hostname*)
                       (jabber-jid-server muc-jid))
      (jabber-qim-api-request-post
       #'(lambda (data conn headers)
           (unless (gethash (jabber-jid-user muc-jid)
                            *jabber-qim-muc-vcard-cache*)
             (puthash (jabber-jid-user muc-jid)
                      (if (and (equal "200" (gethash 'status-code headers))
                               (ignore-errors
                                 (nth 0 (cdr (assoc 'data data)))))
                          (nth 0 (cdr (assoc 'data data)))
                        `((SN . ,(jabber-jid-user muc-jid))
                          (MN . ,(jabber-jid-user muc-jid))))
                      *jabber-qim-muc-vcard-cache*))
           (jabber-muc-join jc
                            muc-jid
                            (jabber-muc-read-my-nickname jc muc-jid t)
                            popup))
       "getmucvcard"
       (json-encode (vector `((:muc_name . ,(jabber-jid-user muc-jid))
                              (:version . 0))))
       'applicaition/json)
    ;; Fallback
    (jabber-muc-join jc muc-jid
                     (jabber-muc-read-my-nickname jc muc-jid)
                     popup)))

(defun jabber-qim-muc-accept-invite (xml-data who mode)
  "Accept QIM MUC invitation automatically"
  (dolist (x (jabber-xml-get-children xml-data 'x))
    (when (string= (jabber-xml-get-attribute x 'xmlns) "http://jabber.org/protocol/muc#user")
      (let ((invitation (car (jabber-xml-get-children x 'invite)))
            (group (jabber-xml-get-attribute xml-data 'from)))
        (when (and invitation
                   (string-prefix-p (format "%s." *jabber-qim-muc-sub-hostname*)
                                    (jabber-jid-server group)))
          (jabber-qim-muc-join jabber-buffer-connection group)
          (return t))))))


;;;###autoload (autoload 'jabber-qim-get-muc-vcard "jabber-qim-extension" "Return MUC vcard" t)
(defun jabber-qim-get-muc-vcard (muc-jid)
  "Return MUC vcard"
  (and
   (string-prefix-p (format "%s." *jabber-qim-muc-sub-hostname*)
                    (jabber-jid-server muc-jid))
   (or (gethash (jabber-jid-user muc-jid) *jabber-qim-muc-vcard-cache*)
       (lexical-let ((latch (make-one-time-latch))
                     (vcard nil))
         (jabber-qim-api-request-post
          (lambda (data conn headers)
            (ignore-errors
              (setq vcard
                    (nth 0 (cdr (assoc 'data data))))
              )
            (apply-partially #'nofify latch))
          "getmucvcard"
          (json-encode (vector `((:muc_name . ,(jabber-jid-user muc-jid))
                                 (:version . 0))))
          'applicaition/json)
         (wait latch 0.2)
         (if (null vcard)
             (puthash (jabber-jid-user muc-jid)
                      `((SN . ,(jabber-jid-user muc-jid))
                        (MN . ,(jabber-jid-user muc-jid)))
                      *jabber-qim-muc-vcard-cache*)
           (puthash (jabber-jid-user muc-jid) vcard *jabber-qim-muc-vcard-cache*))
         vcard))))


(defun jabber-qim-parse-object-attribute (text attribute)
  "Parse object attribute from objects like [obj type=\"emoticon\" value=\"[/guzg]\"]"
  (string-match (format "%s=\\\".*\\\"" attribute) text)
  (let ((type-text (match-string 0 text)))
    (string-match "\\\".*?\\\"" type-text)
    (replace-regexp-in-string "\\\"" "" (match-string 0 type-text))
    ))

(defvar *jabber-qim-emotion-map*
  (make-hash-table :test 'equal))

(defun jabber-qim-get-emotion-by-shortcut (shortcut)
  (gethash shortcut *jabber-qim-emotion-map*))

(defun jabber-qim-load-emotions-from-dir (dir)
  "Load emotions from single directory"
  (let ((resource-xml (xml-parse-file
                       (format "%s/emotions.xml" dir))))
    (mapcar (lambda (face-node)
              (let* ((face-attributes (nth 1 face-node))
                     (face-shortcut (cdr (assoc 'shortcut face-attributes)))
                     (file-org (caddr (nth 3 face-node)))
                     (file-fixed (caddr (nth 5 face-node))))
                (puthash face-shortcut
                         (format "%s/%s" dir file-org)
                         *jabber-qim-emotion-map*)
                ))
            (remove-if-not 'listp
                           (cdddr
                            (cadddr
                             (assoc 'FACESETTING
                                    resource-xml))))))
  )

(defun jabber-qim-load-emotions (emotion-base-dir)
  (mapcar 'jabber-qim-load-emotions-from-dir
          (mapcar #'(lambda (dir)
                      (expand-file-name dir emotion-base-dir))
                  (remove-if #'(lambda (dir)
                                 (or (equal dir ".")
                                     (equal dir "..")))
                             (directory-files emotion-base-dir)))))

(jabber-qim-load-emotions (expand-file-name jabber-qim-local-emotions-directory))

(defun jabber-qim-emotion-image (shortcut)
  (jabber-create-image (jabber-qim-get-emotion-by-shortcut shortcut)))


(defun secure-hash-file (file algorithm)
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (secure-hash algorithm (current-buffer)))))


(defun jabber-qim-view-file-in-directory (file-path)
  (find-file (file-name-directory file-path))
  (revert-buffer t t t)
  (dired-goto-file file-path))

(defun jabber-qim-forward-object-action (button)
  (let* ((session-muc-alist (jabber-qim-session-muc-vcard-alist))
         (jid (jabber-read-jid-completing "Forward to: "
                                          (append (mapcar #'car session-muc-alist)
                                                  *jabber-qim-user-jid-cache*)))
         (jc (jabber-read-account))
         (muc-jid (cdr (assoc (intern jid)
                              session-muc-alist)))
         (send-function (if muc-jid
                            'jabber-muc-send
                          'jabber-chat-send))
         (buffer (if muc-jid
                     (jabber-muc-create-buffer jc muc-jid)
                   (jabber-chat-create-buffer jc jid))))
    (switch-to-buffer buffer)
    (funcall send-function
             jc
             (button-get button :object-text)
             (button-get button :msg-type))
    ))

(defun jabber-qim-insert-file (file-desc body-text face)
  "Insert file into chat buffer."
  (insert "\n\n")
  (insert (jabber-propertize
           (format "[File Received: %s; Size: %s; MD5 Checksum: %s] "
                   (cdr (assoc 'FileName
                               file-desc))
                   (cdr (assoc 'FileSize
                               file-desc))
                   (cdr (assoc 'FILEMD5
                               file-desc)))
           'face face))
  (insert "\n")
  (insert-button "View In Directory"
                 :file-desc file-desc
                 'action #'(lambda (button)
                             (lexical-let* ((file-name (cdr (assoc 'FileName
                                                                   (button-get button :file-desc))))
                                            (file-path (format "%s/%s"
                                                               (jabber-qim-local-received-files-cache-dir)
                                                               file-name))
                                            (file-md5 (cdr (assoc 'FILEMD5
                                                                  (button-get button :file-desc))))
                                            (url (format "%s/%s" *jabber-qim-file-server*
                                                         (cdr (assoc 'HttpUrl
                                                                     (button-get button :file-desc))))))
                               (if (and
                                    (file-exists-p file-path)
                                    (string= file-md5 (secure-hash-file file-path 'md5)))
                                   (jabber-qim-view-file-in-directory file-path)
                                 (web-http-get
                                  #'(lambda (httpc header body)
                                      (let ((coding-system-for-write 'binary))
                                        (with-temp-file file-path
                                          (insert body))
                                        (message "File %s downloaded" file-name)
                                        (jabber-qim-view-file-in-directory file-path)))
                                  :url url)))))
  (insert "\t")
  (insert-button "Forward File To..."
                 :object-text body-text
                 :msg-type jabber-qim-msg-type-file
                 'action #'jabber-qim-forward-object-action)
  (insert "\n"))


(defun jabber-qim-insert-object (object-text face)
  "Insert object into chat buffer."
  (let ((type (intern (jabber-qim-parse-object-attribute object-text "type")))
        (value (jabber-qim-parse-object-attribute object-text "value")))
    (case type
      ('emoticon
       (let ((image (jabber-qim-emotion-image
                     (replace-regexp-in-string "\\\]" ""
                                               (replace-regexp-in-string "\\\[" "" value)))))
         (if image
             (insert-image
                image
                value)
           (insert (jabber-propertize
                    object-text
                    'face face)))))
      ('image
       (insert "\n\n")
       (let* ((image-ret (jabber-qim-load-image value))
              (image (cadr image-ret))
              (image-md5 (car image-ret)))
         (if image
             (progn
               (insert-image
                image
                value)
               (insert "\n\n")
               (insert-button "View Image"
                      :image-md5 image-md5
                      :image-ext (car (last (split-string value "[.]")))
                      'action #'(lambda (button)
                                  (let ((file-path (format "%s/%s.%s"
                                                           (jabber-qim-local-images-cache-dir)
                                                           (button-get button :image-md5)
                                                           (button-get button :image-ext))))
                                    (when (file-exists-p file-path)
                                      (find-file file-path)
                                      (read-only-mode))))))
           (progn
             (insert (jabber-propertize
                (format "[Image]<%s/%s> " *jabber-qim-file-server* value)
                'face face))
             (insert-button "View Image"
                      :image-url (format "%s/%s" *jabber-qim-file-server* value)
                      :image-ext (car (last (split-string value "[.]")))
                      'action #'(lambda (button)
                                  (lexical-let ((image-url (button-get button :image-url))
                                                (image-ext (button-get button :image-ext)))
                                    (web-http-get
                                     #'(lambda (httpc header body)
                                         (ignore-errors
                                           (when (equal "200" (gethash 'status-code header))
                                             (let ((file-path (format "%s/%s.%s"
                                                                      (jabber-qim-local-images-cache-dir)
                                                                      (md5 body)
                                                                      image-ext)))
                                               (unless (file-exists-p file-path)
                                                 (let ((coding-system-for-write 'binary))
                                                   (with-temp-file file-path
                                                     (insert body))))
                                               (find-file file-path)
                                               (read-only-mode)
                                               ))))
                                     :url image-url))))
             )))
       (insert "\t")
       (insert-button "Forward Image To..."
                      :object-text object-text
                      :msg-type jabber-qim-msg-type-default
                      'action #'jabber-qim-forward-object-action)
       (insert "\n\n"))
      ('url
       (insert (jabber-propertize
                value
                'face face)))
      (t
       (insert (jabber-propertize
                object-text
                'face face))))))


(defun jabber-qim-load-image (url-path)
  (lexical-let ((latch (make-one-time-latch))
                (image nil)
                (ret nil))
    (web-http-get
     #'(lambda (httpc header body)
         (ignore-errors
           (when (equal "200" (gethash 'status-code header))
             (let ((file-path (format "%s/%s.%s"
                                      (jabber-qim-local-images-cache-dir)
                                      (md5 body)
                                      (car (last (split-string url-path "[.]"))))))
               (unless (file-exists-p file-path)
                 (let ((coding-system-for-write 'binary))
                   (with-temp-file file-path
                     (insert body))))
               (setq image file-path))))
         (setq ret (md5 body))
         (apply-partially #'nofify latch))
     :url (format "%s/%s" *jabber-qim-file-server* url-path)
     )
    (wait latch 0.5)
    (when image
      (list ret (jabber-create-image image)))))

(defun jabber-qim-load-file (file-desc)
  (lexical-let ((file-path (format "%s/%s"
                           (jabber-qim-local-received-files-cache-dir)
                           (cdr (assoc 'FileName file-desc))))
        (url (format "%s/%s" *jabber-qim-file-server* (cdr (assoc 'HttpUrl file-desc)))))
    (web-http-get
     #'(lambda (httpc header body)
         (let ((coding-system-for-write 'binary))
             (with-temp-file file-path
               (insert body))))
     :url url
     )
    `((:saved-path . ,file-path)
      (:filename . ,(cdr (assoc 'FileName file-desc)))
      (:link . ,url)
      (:size . ,(cdr (assoc 'FileSize file-desc)))
      (:md5 . ,(cdr (assoc 'FILEMD5 file-desc))))
    ))

(defconst jabber-qim-msg-type-file "5"
  "Message is a file")

(defconst jabber-qim-msg-type-default "1"
  "Normal messages")

(defconst jabber-qim-max-send-file-size (* 10 1024 1024)
  "Max send file size set to 10MB")


(cl-defun jabber-qim-send-file (jc jid filename send-function &optional chat-buffer)
  (if (<= (nth 7 (file-attributes filename))
          jabber-qim-max-send-file-size)
      (let ((file-buffer (find-file-noselect filename t t)))
        (web-http-post
         #'(lambda (httpc headers body)
             (let ((jabber-group jid)
                   (jabber-chatting-with jid)
                   (image (ignore-errors
                            (create-image filename))))
               (when chat-buffer
                 (switch-to-buffer chat-buffer))
               (funcall send-function jc
                        (if image
                            (let ((size (image-size image)))
                              (format "[obj type=\"image\" value=\"%s\" width=%s height=%s]"
                                      (string-trim (url-unhex-string body))
                                      (car size)
                                      (cdr size)))
                          (json-encode `((:HttpUrl . ,(string-trim (url-unhex-string body)))
                                         (:FileName . ,(file-name-nondirectory filename))
                                         (:FILEID . ,(jabber-message-uuid))
                                         (:FILEMD5 . ,(secure-hash-file filename 'md5))
                                         (:FileSize . ,(file-size-human-readable
                                                        (nth 7 (file-attributes filename)))))))
                        (if image
                            jabber-qim-msg-type-default
                          jabber-qim-msg-type-file))))
         :url (format "%s/cgi-bin/file_upload.pl" *jabber-qim-file-server*)
         :mime-type 'multipart/form-data
         :data `(("file" . ,file-buffer)))
        (kill-buffer file-buffer))
    ))

(defvar *jabber-qim-user-muc-room-jid-list*
  '())

(defun jabber-qim-user-muc-preload (jc)
  (setq *jabber-qim-user-muc-room-jid-list* '())
  (jabber-send-iq jc (format "%s.%s"
                             *jabber-qim-muc-sub-hostname*
                             *jabber-qim-hostname*)
                  "get"
                  '(query ((xmlns . "http://jabber.org/protocol/muc#user_mucs")))
                  #'(lambda (jc xml-data closure-data)
                      (mapcar #'(lambda (muc)
                                  (let ((muc-jid (format "%s@%s"
                                                       (cdr (assoc 'name muc))
                                                       (cdr (assoc 'host muc)))))
                                    (jabber-qim-api-request-post
                                     #'(lambda (data conn headers)
                                         (let ((muc-vcard (if (and (equal "200" (gethash 'status-code headers))
                                                               (ignore-errors
                                                                 (nth 0 (cdr (assoc 'data data)))))
                                                          (nth 0 (cdr (assoc 'data data)))
                                                          `((SN . ,(jabber-jid-user muc-jid))
                                                            (MN . ,(jabber-jid-user muc-jid))))))
                                           (add-to-list '*jabber-qim-user-muc-room-jid-list*
                                                        (cons (intern (jabber-qim-muc-vcard-group-display-name muc-vcard))
                                                              (jabber-qim-muc-vcard-group-jid muc-vcard)))))
                                     "getmucvcard"
                                     (json-encode (vector `((:muc_name . ,(jabber-jid-user muc-jid))
                                                            (:version . 0))))
                                     'applicaition/json)))
                              (mapcar #'cadr (cddar (jabber-xml-get-children xml-data 'query)))))
                  nil
                  #'(lambda (jc xml-data closure-data)
                      (message "%s" closure-data))
                  "MUC preload failed"))


(add-to-list 'jabber-post-connect-hooks 'jabber-qim-user-muc-preload)


(defun jabber-qim-muc-send-file (jc group filename)
  (interactive
   (jabber-muc-argument-list
    (list (read-file-name "Send File or Image: "))))
  (if (<= (nth 7 (file-attributes filename))
          jabber-qim-max-send-file-size)
      (jabber-qim-send-file jc group filename 'jabber-muc-send)
    (error "File size exceeds maximum: %s"
           (file-size-human-readable jabber-qim-max-send-file-size))))

(defun jabber-qim-chat-send-file (jc chat-with filename &optional chat-buffer)
  (interactive
   (list (jabber-read-account)
         jabber-chatting-with
         (read-file-name "Send File or Image: ")
         (current-buffer)))
  (if chat-with
      (if (<= (nth 7 (file-attributes filename))
              jabber-qim-max-send-file-size)
          (jabber-qim-send-file jc chat-with filename 'jabber-chat-send chat-buffer)
        (error "File size exceeds maximum: %s"
           (file-size-human-readable jabber-qim-max-send-file-size)))
    (error "Not in CHAT buffer")))


(defun jabber-qim-chat-start-groupchat (jc chat-with invited-members groupchat-name)
  (interactive
   (list (jabber-read-account)
         (when (bound-and-true-p jabber-chatting-with)
           jabber-chatting-with)
         (let ((initial-members '())
               (invited nil))
           (while (> (length (setq invited
                                   (completing-read "Invite (leave blank for end of input): "
                                                    *jabber-qim-user-jid-cache*)))
                     0)
             (add-to-list 'initial-members invited))
           initial-members)
         (read-string "New Group Name: "
                      (ignore-errors
                        (format "%s,%s"
                                (jabber-qim-jid-nickname (plist-get (fsm-get-state-data jabber-buffer-connection) :original-jid))
                                (jabber-qim-jid-nickname jabber-chatting-with)))
                      nil nil t)))
  (let* ((my-jid (plist-get (fsm-get-state-data jabber-buffer-connection) :original-jid))
         (muc-jid (format "%s@%s.%s"
                          (secure-hash 'md5 (format "%s,%s,%s,%s"
                                                    my-jid
                                                    groupchat-name
                                                    chat-with
                                                    (format-time-string "%s")))
                          *jabber-qim-muc-sub-hostname*
                          *jabber-qim-hostname*)))
    (when (or invited-members
              chat-with)
      (puthash muc-jid
               (append (when (not (null chat-with))
                         (list chat-with))
                       invited-members)
               *jabber-qim-muc-initial-members*))
    (jabber-qim-api-request-post
     (lambda (data conn headers)
       (when (equal "200" (gethash 'status-code headers))
         (puthash (jabber-jid-user muc-jid)
                  `((SN . ,groupchat-name)
                    (MN . ,(jabber-jid-user muc-jid)))
                  *jabber-qim-muc-vcard-cache*)
         (jabber-qim-muc-join jc muc-jid t)))
     "setmucvcard"
     (json-encode (vector `((:muc_name . ,(jabber-jid-user muc-jid))
                            (:nick . ,groupchat-name))))
     'applicaition/json)))

(defun jabber-qim-body-parse-file (body)
  (let ((file-desc (ignore-errors
                     (json-read-from-string body))))
    (when (and file-desc
               (cdr (assoc 'FileName file-desc))
               (cdr (assoc 'HttpUrl file-desc)))
      file-desc)))

(provide 'jabber-qim-extension)

