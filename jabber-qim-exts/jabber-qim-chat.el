;;; QTalk extension for basic chats -*- lexical-binding: t -*-

;; jabber-qim-chat.el 

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


(require 'json)
(require 'jabber-qim-util)
(require 'jabber-qim-webapi)
(require 'jabber-core)
(require 'jabber-avatar)
(require 'jabber-util)

;; user environment

;;;###autoload
(defvar *jabber-qim-user-vcard-cache*
  (make-hash-table :test 'equal))

;;;###autoload
(defvar *jabber-qim-user-jid-cache*
  '())

;;;###autoload
(defvar *jabber-qim-username-to-jid-cache*
  '())

(defconst jabber-qim-user-vcard-reload-cycle
  86400)

(defun jabber-qim-users-preload (jc)
  (message "Reloading user vcards...")
  (jabber-connect-all)
  (when (jabber-connection-original-jid jc)
    (jabber-qim-api-request-get
     #'(lambda (data conn headers)
         (setq *jabber-qim-user-jid-cache* '())
         (setq *jabber-qim-username-to-jid-cache* '())
         (clrhash *jabber-qim-user-vcard-cache*)
         (mapcar #'(lambda (vcard)
                     (add-to-list '*jabber-qim-user-jid-cache*
                                  (jabber-jid-symbol (jabber-qim-user-vcard-jid vcard)))
                     (puthash (jabber-qim-user-vcard-jid vcard)
                              vcard *jabber-qim-user-vcard-cache*)
                     (add-to-list '*jabber-qim-username-to-jid-cache*
                                  (cons (intern (format "%s - %s"
                                                        (jabber-qim-user-vcard-name vcard)
                                                        (jabber-qim-user-vcard-position vcard)))
                                        (jabber-qim-user-vcard-jid vcard))))
                 data))
     "getusers"
     (jabber-qim-api-connection-auth-info jc))))

(defvar
  *jabber-qim-user-vcards-current-version*
  0)

(defun jabber-qim-users-incremental-preload (jc)
  (when (jabber-connection-original-jid jc)
    (jabber-qim-api-request-post
     #'(lambda (response-data conn headers)
         (when (and
                (equal "200" (gethash 'status-code headers))
                (cdr (assoc 'ret response-data)))
           (let* ((data (cdr (assoc 'data response-data)))
                  (updated-users (cdr (assoc 'update data)))
                  (deleted-users (cdr (assoc 'delete data)))
                  (new-version (cdr (assoc 'version data))))
             (when (> new-version *jabber-qim-user-vcards-current-version*)
               (message "Reloading user vcards incrementally. Previous version: %d; New version: %d"
                        *jabber-qim-user-vcards-current-version* new-version)
               (mapcar #'(lambda (vcard)
                           (puthash (jabber-qim-user-vcard-jid vcard)
                                    vcard *jabber-qim-user-vcard-cache*))
                       updated-users)
               (mapcar #'(lambda (vcard)
                           (remhash (jabber-qim-user-vcard-jid vcard)
                                    *jabber-qim-user-vcard-cache*))
                       deleted-users)
               ; Reload JID caches:
               (setq *jabber-qim-user-jid-cache* '())
               (setq *jabber-qim-username-to-jid-cache* '())
               (maphash #'(lambda (key vcard)
                            (add-to-list '*jabber-qim-user-jid-cache*
                                         (jabber-jid-symbol (jabber-qim-user-vcard-jid vcard)))
                            (add-to-list '*jabber-qim-username-to-jid-cache*
                                         (cons (intern (format "%s - %s"
                                                               (jabber-qim-user-vcard-name vcard)
                                                               (jabber-qim-user-vcard-position vcard)))
                                               (jabber-qim-user-vcard-jid vcard))))
                        *jabber-qim-user-vcard-cache*)
               (message "User mucs reload complete. Previous version: %d; New version: %d"
                        *jabber-qim-user-vcards-current-version* new-version)
               (setq *jabber-qim-user-vcards-current-version*
                     new-version)))))
     *jabber-qim-webapi-command-update-users*
     (json-encode
      `((version . ,*jabber-qim-user-vcards-current-version*)))
     'application/json
     (jabber-qim-api-connection-auth-info jc))))

;; extension functions
(defun jabber-qim-object-attributes (object-text)
  (when (string-prefix-p "[obj " object-text)
    (mapcar #'(lambda (kv-text)
                (let ((kv (split-string kv-text "=")))
                  (cons (intern (car kv))
                        (replace-regexp-in-string "\\\"" ""
                                                  (string-join (cdr kv) "=")))))
            (split-string (subseq object-text
                                  5 (1- (length object-text)))))))


(defvar *jabber-qim-emotion-map*
  (make-hash-table :test 'equal))

(defun jabber-qim-get-emotion-by-shortcut (shortcut)
  (gethash shortcut *jabber-qim-emotion-map*))

(defun jabber-qim-load-emotions-from-dir (dir)
  "Load emotions from single directory"
  (let ((resource-xml (ignore-errors
                        (xml-parse-file
                         (format "%s/emotions.xml" dir)))))
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



(defun jabber-qim-view-file-in-directory (file-path)
  (find-file (file-name-directory file-path))
  (revert-buffer t t t)
  (dired-goto-file file-path))


(defun jabber-qim-chat-switch-to-unread (jc jid &optional other-window)
  "Switch to an unread message buffer."
  (interactive (let* ((session-muc-alist
                       (jabber-qim-session-muc-vcard-alist))
                      (unread-message-jids
                       (mapcar #'jabber-jid-user jabber-activity-jids))
                      (jid-or-username
                       (jabber-read-jid-completing "Switch To Unread Conversation: "
                                                    (mapcar #'(lambda (id)
                                                                (or (car
                                                                     (find-if #'(lambda (x)
                                                                                  (equal id (cdr x)))
                                                                              (append *jabber-qim-username-to-jid-cache*
                                                                                      session-muc-alist)))
                                                                    (intern id)))
                                                            unread-message-jids)
                                                    t nil nil nil t t))
                      (jid (jabber-qim-user-jid-by-completion jid-or-username))
                      (account
                       (jabber-read-account nil jid)))
                 (list 
                  account jid current-prefix-arg)))
  (let* ((muc-jid (cdr (assoc (intern jid)
                              (jabber-qim-session-muc-vcard-alist))))
         (buffer (if muc-jid
                     (jabber-muc-create-buffer jc muc-jid)
                   (jabber-chat-create-buffer jc jid))))
    (switch-to-buffer buffer)))

(define-key jabber-global-keymap "\C-u" 'jabber-qim-chat-switch-to-unread)


(defun jabber-qim-send-to-chat (data &optional prompt msg-type)
  "Send data to selected (active) chat buffer"
  (let* ((session-muc-alist (jabber-qim-session-muc-vcard-alist))
         (jid (jabber-qim-user-jid-by-completion
               (jabber-read-jid-completing (if (stringp prompt)
                                               prompt
                                             "Select chat buffer: ")
                                           (append (mapcar #'car session-muc-alist)
                                                   (jabber-qim-user-jid-completion-list))
                                           nil
                                           nil
                                           nil
                                           nil
                                           t)))
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
             data
             msg-type)))

(defun jabber-qim-forward-object-action (button)
  (jabber-qim-send-to-chat
   (button-get button :object-text)
   "Forward to: "
   (button-get button :msg-type)))

(defun jabber-qim-object-url (object-value)
  (let ((url-obj (url-generic-parse-url object-value)))
    (if (url-host url-obj)
        object-value
      (format "%s/%s" *jabber-qim-file-server* object-value))))


(defun jabber-qim-insert-file (file-desc body-text face &optional uid)
  "Insert file into chat buffer."
  (let ((file-link (jabber-qim-object-url (cdr (assoc 'HttpUrl file-desc)))))
    (insert "\n\n")
    (insert (jabber-propertize
             (format "[File Received: %s; Size: %s; Link: %s] "
                     (cdr (assoc 'FileName
                                 file-desc))
                     (cdr (assoc 'FileSize
                                 file-desc))
                     file-link)
             'face face))
    (insert "\n")
    (insert-button "View In Directory"
                   :file-desc file-desc
                   :file-link file-link
                   :uid (or uid "")
                   'action #'(lambda (button)
                               (lexical-let* ((file-name (cdr (assoc 'FileName
                                                                     (button-get button :file-desc))))
                                              (file-path (format "%s/%s"
                                                                 (jabber-qim-local-received-files-cache-dir)
                                                                 file-name))
                                              (file-md5 (cdr (assoc 'FILEMD5
                                                                    (button-get button :file-desc))))
                                              (url (button-get button :file-link)))
                                 (if (and
                                      (file-exists-p file-path)
                                      (string= file-md5 (secure-hash-file file-path 'md5)))
                                     (jabber-qim-view-file-in-directory file-path)
                                   (web-http-get
                                    #'(lambda (httpc header body)
                                        (if (equal "200" (gethash 'status-code header))
                                            (let ((coding-system-for-write 'binary))
                                              (with-temp-file file-path
                                                (set-buffer-multibyte nil)
                                                (encode-coding-string body 'utf-8 nil (current-buffer)))
                                              (message "File %s downloaded" file-name)
                                              (jabber-qim-view-file-in-directory file-path))
                                          (message "ERROR Downloading %s: %s %s"
                                                   file-name
                                                   (gethash 'status-code header)
                                                   (gethash 'status-string header))))
                                    :url url)))))
    (insert "\t")
    (insert-button "Forward File To..."
                   :object-text body-text
                   :msg-type jabber-qim-msg-type-file
                   'action #'jabber-qim-forward-object-action)
    (insert "\n")))


(defconst jabber-qim-max-image-width 1024)

(defconst jabber-qim-max-image-height 768)


(defun jabber-qim-scale-image-size (width height)
  (let ((scale (max (/ width jabber-qim-max-image-width)
                    (/ height jabber-qim-max-image-height))))
    (if (> scale 1)
        (cons (round (/ width scale))
              (round (/ height scale)))
      (cons width height))))


(defun jabber-qim-parse-image-filename (img-value)
  (or
   (ignore-errors
     (car
      (last
       (split-string (find-if
                      #'(lambda (param)
                          (or
                           (string-prefix-p "name=" param)
                           (string-prefix-p "file=" param)
                           (string-prefix-p "filename=" param)))
                      (split-string
                       (cadr (split-string
                              img-value
                              "[?]"))
                       "&")) "/"))))
   (car
    (last (split-string
           (car (url-path-and-query
                 (url-generic-parse-url img-value)))
           "/")))))


(defun jabber-qim-parse-image-type (img-value)
  (let ((ext
         (car
          (last (split-string
                 (jabber-qim-parse-image-filename
                  img-value)
                 "[.]")))))
    (when ext
      (downcase ext))))


(defun jabber-qim-insert-object (object-text face &optional uid)
  "Insert object into chat buffer."
  (let* ((object-attributes (jabber-qim-object-attributes object-text))
         (type (intern (cdr (assoc-string 'type object-attributes))))
         (value (cdr (assoc-string 'value object-attributes))))
    (case type
      ('emoticon
       (let ((image (ignore-errors
                      (jabber-qim-emotion-image
                       (replace-regexp-in-string "\\\]" ""
                                                 (replace-regexp-in-string "\\\[" "" value))))))
         (if image
             (insert-image
              image
              value)
           (insert (jabber-propertize
                    object-text
                    'face face)))))
      ('image
       (insert "\n\n")
       (let* ((image-size (ignore-errors
                            (jabber-qim-scale-image-size (string-to-number
                                                          (cdr (assoc-string 'width object-attributes)))
                                                         (string-to-number
                                                          (cdr (assoc-string 'height object-attributes))))))
              (image-url (jabber-qim-object-url value))
              (image-ret (jabber-qim-wget-image image-url image-size uid))
              (image (cadr image-ret)))
         (if image
             (progn
               (insert-image
                image
                value)
               (insert "\n\n")
               (insert-button "View Image"
                              :image-filepath (caddr image-ret)
                              'action #'(lambda (button)
                                          (let ((file-path (button-get button :image-filepath)))
                                            (when (file-exists-p file-path)
                                              (find-file file-path)
                                              (read-only-mode))))))
           (progn
             (insert (jabber-propertize
                (format "[Image]<%s> " image-url)
                'face face))
             (insert-button "View Image"
                      :image-url image-url
                      :image-ext (jabber-qim-parse-image-type value)
                      :uid (or uid "")
                      'action #'(lambda (button)
                                  (lexical-let ((image-url (button-get button :image-url))
                                                (image-ext (button-get button :image-ext))
                                                (cached-image-file (gethash (button-get button :image-url)
                                                                            *jabber-qim-image-file-cache*)))
                                    (if cached-image-file
                                        (progn
                                          (find-file cached-image-file)
                                          (read-only-mode))
                                      (web-http-get
                                       #'(lambda (httpc header body)
                                           (ignore-errors
                                             (if (equal "200" (gethash 'status-code header))
                                                 (let ((file-path (format "%s/%s.%s"
                                                                          (jabber-qim-local-images-cache-dir)
                                                                          (md5 body)
                                                                          image-ext)))
                                                   (unless (file-exists-p file-path)
                                                     (let ((coding-system-for-write 'binary))
                                                       (with-temp-file file-path
                                                         (insert body)))
                                                     (puthash image-url file-path *jabber-qim-image-file-cache*))
                                                   (find-file file-path)
                                                   (read-only-mode))
                                               (message "ERROR Downloading Image: %s %s"
                                                        (gethash 'status-code header)
                                                        (gethash 'status-string header)))))
                                       :url (format "%s&uid=%s"
                                                    image-url
                                                    (url-hexify-string (button-get button :uid)))))))))))
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

(defun jabber-qim-wget-image (url-path &optional image-size uid)
  (let ((image-file (gethash url-path *jabber-qim-image-file-cache*)))
    (unless image-file
      (let* ((image-download-path (format "%s/%s"
                                         (jabber-qim-local-images-cache-dir)
                                         (jabber-qim-parse-image-filename url-path)))
             (image-url-obj (url-generic-parse-url url-path))
             (image-url (if (url-host image-url-obj)
                            url-path
                          (format "%s/%s" *jabber-qim-file-server* url-path))))
        (ignore-errors
          (call-process (executable-find "wget") nil nil nil
                        "-T" "1.0"
                        "-O" image-download-path
                        (if image-size
                            (format (concat "%s"
                                            (if (find ?? image-url)
                                                ""
                                              "?")
                                            "&w=%s&h=%s&uid=%s")
                                    image-url
                                    (car image-size)
                                    (cdr image-size)
                                    (url-hexify-string
                                     (or uid "")))
                          (format (concat "%s"
                                          (if (find ?? image-url)
                                                ""
                                              "?")
                                          "&uid=%s")
                                  image-url
                                  (url-hexify-string
                                   (or uid ""))))))
        (let ((image-file-size (nth 7 (file-attributes image-download-path))))
          (when (and image-file-size
                     (> image-file-size 0))
            (setq image-file image-download-path)
            (puthash url-path image-download-path *jabber-qim-image-file-cache*)
            ))))
    (when image-file
      (list (secure-hash-file image-file 'md5)
            (jabber-create-image image-file)
            image-file))))

(defun jabber-qim-load-file (file-desc)
  (lexical-let ((file-path (format "%s/%s"
                           (jabber-qim-local-received-files-cache-dir)
                           (cdr (assoc 'FileName file-desc))))
                (url (let ((url-obj (url-generic-parse-url (cdr (assoc 'HttpUrl file-desc)))))
                       (if (url-host url-obj)
                           (cdr (assoc 'HttpUrl file-desc))
                         (format "%s/%s" *jabber-qim-file-server* (cdr (assoc 'HttpUrl file-desc)))))))
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

(defun jabber-qim-chat-insert-body (msg-type body extend-info muc-message-p private-message-p message-from who mode)
  (when body
    (when (eql mode :insert)
      (if (and (> (length body) 4)
               (string= (substring body 0 4) "/me "))
          (let ((action (substring body 4))
                (nick (cond
                       ((eq who :local)
                        (plist-get (fsm-get-state-data jabber-buffer-connection) :username))
                       ((or muc-message-p
                            private-message-p)
                        (jabber-jid-resource message-from))
                       (t
                        (jabber-jid-displayname message-from)))))
            (insert (jabber-propertize
                     (concat nick
                             " "
                             action)
                     'face 'jabber-chat-prompt-system)))
        (let ((file-desc
               (and
                (equal msg-type jabber-qim-msg-type-file)
                (jabber-qim-body-parse-file body)))
              (face (case who
                      ((:foreign :muc-foreign) 'jabber-chat-text-foreign)
                      ((:local :muc-local) 'jabber-chat-text-local)))
              (uid (plist-get (fsm-get-state-data jabber-buffer-connection)
                              :original-jid)))
          (if file-desc
              (jabber-qim-insert-file file-desc body face
                                      uid)
            (progn
              (jabber-chat-print-message-body-segments
               body
               face
               uid)
              (when (and extend-info
                         (not (string= msg-type
                                       jabber-qim-msg-type-common-trd-info)))
                (jabber-chat-print-message-body-segments
                 (format "\n\n *******\n%s"
                         extend-info)
                 face
                 uid)))))))
    t))

(defconst jabber-qim-msg-type-muc-notify "15"
  "Message is a groupchat notify")

(defconst jabber-qim-msg-type-redpack (format "%s" (lsh 1 9))
  "Message is a red pack")

(defconst jabber-qim-msg-type-aa (format "%s" (1+ (lsh 1 9)))
  "Message is a aa")

(defconst jabber-qim-msg-type-redpack-info (format "%s" (lsh 1 10))
  "Message is a red pack info")

(defconst jabber-qim-msg-type-aa-info (format "%s" (1+ (lsh 1 10)))
  "Message is a aa info")


(defconst jabber-qim-msg-type-file "5"
  "Message is a file")

(defconst jabber-qim-msg-type-default "1"
  "Normal messages")

(defconst jabber-qim-msg-type-common-trd-info "666"
  "CommonTrdInfo")

(defconst jabber-qim-max-send-file-size (* 100 1024 1024)
  "Max send file size set to 10MB")

(cl-defun jabber-qim-send-file (filename jc jid send-function &optional chat-buffer)
  (interactive
   (append (list (read-file-name (let ((current-jid (or jabber-group
                                                        jabber-chatting-with)))
                                   (if current-jid
                                       (format "Sending File or Image to %s: "
                                               (jabber-jid-displayname current-jid))
                                     "Send File or Image: "))))
           (jabber-qim-interactive-send-argument-list "To chat: ")))
  (if (<= (nth 7 (file-attributes filename))
          jabber-qim-max-send-file-size)
      (let* ((file-buffer (find-file-noselect filename t t))
             (file-hash-code (secure-hash 'md5 file-buffer))
             (image
              (ignore-errors
                (create-image filename)))
             (file-type (if image
                            "img"
                          "file"))
             (connection-state (plist-get jc
                                          :state-data)))
        (web-http-post
         #'(lambda (httpc headers body)
             (let* ((jabber-group jid)
                    (jabber-chatting-with jid)
                    (msg-id (jabber-message-uuid))
                    (response (ignore-errors
                                (json-read-from-string body)))
                    (response-ret (cdr (assoc 'ret response)))
                    (response-file-url (cdr (assoc 'data response))))
               (when chat-buffer
                 (switch-to-buffer chat-buffer))
               (funcall send-function jc
                        (if image
                            (let ((size (image-size image t)))
                              (format "[obj type=\"image\" value=\"%s&msgid=%s\" width=%s height=%s]"
                                      (string-trim (url-filename (url-generic-parse-url response-file-url)))
                                      msg-id
                                      (round (car size))
                                      (round (cdr size))))
                          (json-encode `((:HttpUrl . ,(format "%s&msgid=%s"
                                                              (string-trim (url-filename (url-generic-parse-url response-file-url)))
                                                              msg-id))
                                         (:FileName . ,(file-name-nondirectory filename))
                                         (:FILEID . ,(jabber-message-uuid))
                                         (:FILEMD5 . ,(secure-hash-file filename 'md5))
                                         (:FileSize . ,(file-size-human-readable
                                                        (nth 7 (file-attributes filename)))))))
                        (if image
                            jabber-qim-msg-type-default
                          jabber-qim-msg-type-file)
                        msg-id)))
         :url (format "%s/file/%s/upload/%s?k=%s&u=%s&key=%s&name=%s&size=%s"
                      *jabber-qim-file-server*
                      *jabber-qim-file-service-version*
                      file-type
                      (plist-get connection-state
                                 :qim-auth-key)
                      (plist-get connection-state
                                 :username)
                      file-hash-code
                      (url-encode-url (file-name-nondirectory filename))
                      (nth 7 (file-attributes filename)))
         :mime-type 'multipart/form-data
         :data `(("file" . ,file-buffer)))
        (kill-buffer file-buffer))))

(define-key jabber-global-keymap "\C-f" 'jabber-qim-send-file)


(defun jabber-qim-interactive-send-argument-list (&optional prompt)
  (let* ((jc (jabber-read-account))
         (jid-at-point (or
                        (bound-and-true-p jabber-chatting-with)
                        (bound-and-true-p jabber-group)))
         (session-muc-alist (jabber-qim-session-muc-vcard-alist))
         (jid (or
               jid-at-point
               (jabber-qim-user-jid-by-completion
                (jabber-read-jid-completing (if (stringp prompt)
                                                prompt
                                              "Select chat: ")
                                            (append (mapcar #'car session-muc-alist)
                                                    (jabber-qim-user-jid-completion-list))
                                            nil
                                            nil
                                            nil
                                            nil
                                            t))))
         (muc-jid (if (and
                       jid-at-point
                       (jabber-qim-muc-jid-p jid-at-point))
                      jid-at-point
                    (cdr (assoc (intern jid)
                                session-muc-alist))))
         (send-function (if muc-jid
                            'jabber-muc-send
                          'jabber-chat-send))
         (buffer (if muc-jid
                     (jabber-muc-create-buffer jc muc-jid)
                   (jabber-chat-create-buffer jc jid))))
    (list jc
          (or muc-jid
              jid)
          send-function
          buffer)))

(defun jabber-qim-send-screenshot (jc jid send-function &optional chat-buffer)
  (interactive
   (jabber-qim-interactive-send-argument-list "Send screenshot to chat: "))
  (let ((image-file (format "%s/%s.png"
                            (jabber-qim-local-screenshots-dir)
                            (jabber-message-uuid)))
        (screencapture-executable (executable-find
                                   (if (eq system-type 'darwin)
                                       "screencapture"
                                     "import")))
        (current-jid (or jabber-group
                         jabber-chatting-with)))
    (if screencapture-executable
        (progn
          (when current-jid
            (message "Sending screenshot to %s:" (jabber-jid-displayname current-jid)))
          (if (equal 0 (ignore-errors
                         (if (eq system-type 'darwin)
                             (call-process screencapture-executable nil nil nil
                                           "-i" image-file)
                           (call-process screencapture-executable nil nil nil
                                         image-file))))
              (jabber-qim-send-file image-file jc jid send-function chat-buffer)
            (message "Screen capture failed.")))
      (message "Screen capture exec not available."))))

(define-key jabber-global-keymap "\C-s" 'jabber-qim-send-screenshot)

(defun jabber-qim-send-link (link-text jc jid send-function &optional chat-buffer)
  (interactive
   (append (list (read-string "Send link: "))
           (jabber-qim-interactive-send-argument-list "To chat: ")))
  (funcall send-function jc
           (format "[obj type=\"url\" value=\"%s\"]" link-text)))

(define-key jabber-global-keymap "\C-i" 'jabber-qim-send-link)


(defun jabber-qim-chat-send-screenshot (jc chat-with &optional chat-buffer)
  (interactive
   (list (jabber-read-account)
         jabber-chatting-with
         (current-buffer)))
  (if chat-with
      (jabber-qim-send-screenshot jc chat-with 'jabber-chat-send chat-buffer)
    (error "Not in CHAT buffer")))


(defun jabber-qim-chat-send-file (jc chat-with filename &optional chat-buffer)
  (interactive
   (list (jabber-read-account)
         jabber-chatting-with
         (read-file-name "Send File or Image: ")
         (current-buffer)))
  (if chat-with
      (if (<= (nth 7 (file-attributes filename))
              jabber-qim-max-send-file-size)
          (jabber-qim-send-file filename jc chat-with 'jabber-chat-send chat-buffer)
        (error "File size exceeds maximum: %s"
           (file-size-human-readable jabber-qim-max-send-file-size)))
    (error "Not in CHAT buffer")))

(defun jabber-qim-chat-start-groupchat (jc
                                        chat-with
                                        invited-members
                                        &optional default-groupchat-name)
  (interactive
   (list (jabber-read-account)
         (when (bound-and-true-p jabber-chatting-with)
           jabber-chatting-with)
         (let ((initial-invites '())
               (invited nil))
           (while (> (length (setq invited
                                   (jabber-qim-user-jid-by-completion
                                    (jabber-read-jid-completing "Invite (leave blank for end of input): "
                                                                (jabber-qim-user-jid-completion-list)
                                                                nil nil nil nil t t))))
                     0)
             (add-to-list 'initial-invites invited))
           initial-invites)
         ))
  (let* ((chatroom-members
          (delete-dups
           (-filter #'(lambda (x)
                        x)
                    (append (list (jabber-qim-jid-nickname (plist-get
                                                            (fsm-get-state-data jabber-buffer-connection)
                                                            :original-jid))
                                  (jabber-qim-jid-nickname jabber-chatting-with))
                            (mapcar #'jabber-qim-jid-nickname invited-members)))))
         (groupchat-name
          (or default-groupchat-name
              (read-string "New Group Name: "
                           (string-join
                            (if (> (length chatroom-members) 4)
                                (append (subseq
                                         chatroom-members
                                         0 4)
                                        (list "..."))
                              chatroom-members)
                            ","            
                            )                      
                           nil nil t)))
         (my-jid (plist-get (fsm-get-state-data jabber-buffer-connection) :original-jid))
         (muc-jid (format "%s@%s.%s"
                          (secure-hash 'md5 (format "%s,%s,%s,%s"
                                                    my-jid
                                                    groupchat-name
                                                    chat-with
                                                    (format-time-string "%s")))
                          *jabber-qim-muc-sub-hostname*
                          *jabber-qim-domain*)))
    (when (or invited-members
              chat-with)
      (puthash muc-jid
               (append (when chat-with
                         (list chat-with))
                       invited-members)
               *jabber-qim-muc-initial-members*))
    (jabber-send-iq jc muc-jid
                    "set"
                    '(query ((xmlns . "http://jabber.org/protocol/create_muc")))
                    (lambda (jc xml-data context)
                      (jabber-qim-api-request-post
                       (lambda (response conn headers)
                         (when (and
                                (equal "200" (gethash 'status-code headers))
                                (cdr (assoc 'ret response)))
                           (let ((vcard (cadr (assoc 'data response))))
                             (puthash (jabber-jid-user muc-jid)
                                      `((MN . ,(cdr (assoc 'muc_name vcard)))
                                        (SN . ,(decode-coding-string
                                                (cdr (assoc 'show_name vcard))
                                                'utf-8-emacs-unix)))
                                      *jabber-qim-muc-vcard-cache*))                           
                           (jabber-qim-muc-join jc muc-jid t)))
                       *jabber-qim-webapi-command-set-muc-vcard*
                       (json-encode (vector `((:muc_name . ,(jabber-jid-user (cdr (assoc :muc-jid context))))
                                              (:nick . ,(cdr (assoc :groupchat-name context))))))
                       'application/json
                       (jabber-qim-api-connection-auth-info jc)))
                    `((:muc-jid . ,muc-jid)
                      (:groupchat-name . ,groupchat-name))
                    nil nil)
    ))

(define-key jabber-global-keymap "\C-g" 'jabber-qim-chat-start-groupchat)

(defun jabber-qim-message-type (message)
  (cdr (assoc 'msgType (jabber-xml-node-attributes
                        (car
                         (jabber-xml-get-children message 'body))))))

(defun jabber-qim-body-parse-file (body)
  (let ((file-desc (ignore-errors
                     (json-read-from-string body))))
    (when (and file-desc
               (cdr (assoc 'FileName file-desc))
               (cdr (assoc 'HttpUrl file-desc)))
      file-desc)))

(provide 'jabber-qim-chat)
