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

(defun jabber-qim-local-screenshots-dir ()
  (format "%s/screenshots" jabber-qim-local-file-dir))


(ignore-errors
  (dired-create-directory jabber-qim-local-file-dir))

(ignore-errors
  (dired-create-directory
   (jabber-qim-local-images-cache-dir)))

(ignore-errors
  (dired-create-directory
   (jabber-qim-local-received-files-cache-dir)))

(ignore-errors
  (dired-create-directory (jabber-qim-local-screenshots-dir)))


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

;;;###autoload
(defvar *jabber-qim-username-to-jid-cache*
  '())

;;;###autoload
(defvar *jabber-qim-image-file-cache*
  (make-hash-table :test 'equal))

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


(jabber-qim-api-request-post
 #'(lambda (data conn headers)
     (mapcar #'(lambda (vcard)
                 (add-to-list '*jabber-qim-user-jid-cache*
                              (jabber-jid-symbol (jabber-qim-user-vcard-jid vcard)))
                 (puthash (jabber-qim-user-vcard-jid vcard)
                          vcard *jabber-qim-user-vcard-cache*)
                 (add-to-list '*jabber-qim-username-to-jid-cache*
                              (cons (intern (format "%s - %s"
                                                    (jabber-qim-user-vcard-name vcard)
                                                    (jabber-qim-user-vcard-position vcard)))
                                    (jabber-qim-user-vcard-jid vcard)))) data))
 "getusers"
 "u="
 'applicaion/json)

;;;###autoload (autoload 'jabber-qim-jid-nickname "jabber-qim-extension" "Return user nickname" t)
(defun jabber-qim-jid-nickname (jid)
  "Return user nickname"
  (when jid
    (let ((user-vcard (gethash (jabber-jid-user jid)
                               *jabber-qim-user-vcard-cache*)))
      (when user-vcard
        (jabber-qim-user-vcard-name user-vcard)))))


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

(defconst *qim-autojoin-settings-file*
  "~/.qim-autojoin.el")

(when (file-exists-p *qim-autojoin-settings-file*)
  (ignore-errors
    (load-file *qim-autojoin-settings-file*)))

(defun jabber-qim-save-qim-muc-autojoin-settings ()
  (interactive)
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file *qim-autojoin-settings-file*
      (insert (format "(setq jabber-qim-muc-autojoin '%s)"
                      (prin1-to-string jabber-qim-muc-autojoin)))))
  t)

(add-hook 'jabber-post-disconnect-hook
          'jabber-qim-save-qim-muc-autojoin-settings)

(add-to-list 'kill-emacs-query-functions
		     'jabber-qim-save-qim-muc-autojoin-settings)


(defun jabber-qim-muc-toggle-autojoin ()
  (interactive)
  (when jabber-group
    (if (find-if #'(lambda (x)
                     (string=  jabber-group (car x)))
                 jabber-qim-muc-autojoin)
        (when (y-or-n-p (format "Remove %s from autojoin list?"
                                (jabber-jid-displayname jabber-group)))
          (setq jabber-qim-muc-autojoin
                (remove-if #'(lambda (x)
                               (string= (car x) jabber-group))
                           jabber-qim-muc-autojoin)))
      (when (y-or-n-p (format "Add %s to autojoin list?"
                              (jabber-jid-displayname jabber-group)))
        (add-to-list 'jabber-qim-muc-autojoin (list jabber-group
                                                    (cons :silence (not (y-or-n-p "Enable message alerts?")))))))))

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
                (jabber-read-jid-completing "Join group: "
                                            (mapcar #'car
                                                    (-filter #'(lambda (muc)
                                                                 (not (find (cdr muc)
                                                                            (mapcar #'car *jabber-active-groupchats*)
                                                                            :test 'string=)))
                                                             *jabber-qim-user-muc-room-jid-list*))
                                            nil nil nil nil t)))
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

(define-key jabber-global-keymap "\C-m" 'jabber-qim-muc-join)


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
          (add-to-list 'jabber-qim-muc-autojoin (list group
                                                      (cons :silence t)))
          (return t))))))


;;;###autoload (autoload 'jabber-qim-get-muc-vcard "jabber-qim-extension" "Return MUC vcard" t)
(defun jabber-qim-get-muc-vcard (muc-jid)
  "Return MUC vcard"
  (and
   muc-jid
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
                                      (if (equal "200" (gethash 'status-code header))
                                          (let ((coding-system-for-write 'binary))
                                            (with-temp-file file-path
                                              (insert body))
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
  (insert "\n"))

(defconst jabber-qim-max-image-width 1024)

(defconst jabber-qim-max-image-height 1024)


(defun jabber-qim-scale-image-size (width height)
  (let ((scale (max (/ width jabber-qim-max-image-width)
                    (/ height jabber-qim-max-image-height))))
    (if (> scale 1)
        (cons (round (/ width scale))
              (round (/ height scale)))
      (cons width height))))

(defun jabber-qim-parse-image-type (img-value)
  (car
   (last
    (split-string (find-if
                   #'(lambda (param)
                       (string-prefix-p "file=" param))
                   (split-string
                    (cadr (split-string
                           img-value
                           "[?]"))
                    "&")) "[.]"))))


(defun jabber-qim-insert-object (object-text face)
  "Insert object into chat buffer."
  (let* ((object-attributes (jabber-qim-object-attributes object-text))
         (type (intern (cdr (assoc-string 'type object-attributes))))
         (value (cdr (assoc-string 'value object-attributes))))
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
       (let* ((image-size (jabber-qim-scale-image-size (string-to-number
                                                        (cdr (assoc-string 'width object-attributes)))
                                                       (string-to-number
                                                        (cdr (assoc-string 'height object-attributes)))))
              (image-ret (jabber-qim-load-image value image-size))
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
                      :image-ext (jabber-qim-parse-image-type value)
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
                      :image-ext (jabber-qim-parse-image-type value)
                      'action #'(lambda (button)
                                  (lexical-let ((image-url (button-get button :image-url))
                                                (image-ext (button-get button :image-ext))
                                                (cached-image-file (gethash (button-get button :image-url)
                                                                            *jabber-qim-image-file-cache*)))
                                    (if cached-image-file
                                        (progn
                                          (message "Found image in cache")
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
                                       :url image-url))))))))
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


(defun jabber-qim-load-image (url-path &optional image-size)
  (lexical-let ((latch (make-one-time-latch))
                (image nil)
                (ret nil))
    (unless (setq image (gethash url-path *jabber-qim-image-file-cache*))
      (web-http-get
       #'(lambda (httpc header body)
           (ignore-errors
             (when (and body
                        (equal "200" (gethash 'status-code header)))
               (let ((file-path (format "%s/%s.%s"
                                        (jabber-qim-local-images-cache-dir)
                                        (md5 body)
                                        (jabber-qim-parse-image-type url-path))))
                 (unless (file-exists-p file-path)
                   (let ((coding-system-for-write 'binary))
                     (with-temp-file file-path
                       (insert body))))
                 (setq image file-path)
                 (puthash url-path file-path *jabber-qim-image-file-cache*)
                 (setq ret (md5 body)))))
           (apply-partially #'nofify latch))
       :url (if image-size
                (format "%s/%s&w=%s&h=%s"
                        *jabber-qim-file-server* url-path
                        (car image-size)
                        (cdr image-size))
              (format "%s/%s" *jabber-qim-file-server* url-path)))
      (wait latch 0.5))
    (when image
      (list (secure-hash-file image 'md5)
            (jabber-create-image image)))))

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


(cl-defun jabber-qim-send-file (filename jc jid send-function &optional chat-buffer)
  (interactive
   (append (list (read-file-name "Send File or Image: "))
           (jabber-qim-interactive-send-argument-list "To chat: ")))
  (if (<= (nth 7 (file-attributes filename))
          jabber-qim-max-send-file-size)
      (let ((file-buffer (find-file-noselect filename t)))
        (web-http-post
         #'(lambda (httpc headers body)
             (let ((jabber-group jid)
                   (jabber-chatting-with jid)
                   (image (ignore-errors
                            (create-image filename)))
                   (msg-id (jabber-message-uuid)))
               (when chat-buffer
                 (switch-to-buffer chat-buffer))
               (funcall send-function jc
                        (if image
                            (let ((size (image-size image t)))
                              (format "[obj type=\"image\" value=\"%s&msgid=%s\" width=%s height=%s]"
                                      (string-trim (url-unhex-string body))
                                      msg-id
                                      (round (car size))
                                      (round (cdr size))))
                          (json-encode `((:HttpUrl . ,(string-trim (url-unhex-string body)))
                                         (:FileName . ,(file-name-nondirectory filename))
                                         (:FILEID . ,(jabber-message-uuid))
                                         (:FILEMD5 . ,(secure-hash-file filename 'md5))
                                         (:FileSize . ,(file-size-human-readable
                                                        (nth 7 (file-attributes filename)))))))
                        (if image
                            jabber-qim-msg-type-default
                          jabber-qim-msg-type-file)
                        msg-id)))
         :url (format "%s/cgi-bin/file_upload.pl" *jabber-qim-file-server*)
         :mime-type 'multipart/form-data
         :data `(("file" . ,file-buffer)))
        (kill-buffer file-buffer))))

(define-key jabber-global-keymap "\C-f" 'jabber-qim-send-file)

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
                            (jabber-message-uuid))))
    (if (equal 0 (ignore-errors
                   (call-process (executable-find "import") nil nil nil image-file)))
        (jabber-qim-send-file image-file jc jid send-function chat-buffer)
      (error "Screen capture failed."))))

(define-key jabber-global-keymap "\C-s" 'jabber-qim-send-screenshot)


(defun jabber-qim-muc-send-screenshot (jc group)
  (interactive
   (jabber-muc-argument-list))
  (jabber-qim-send-screenshot jc group 'jabber-muc-send))

(defun jabber-qim-chat-send-screenshot (jc chat-with &optional chat-buffer)
  (interactive
   (list (jabber-read-account)
         jabber-chatting-with
         (current-buffer)))
  (if chat-with
      (jabber-qim-send-screenshot jc chat-with 'jabber-chat-send chat-buffer)
    (error "Not in CHAT buffer")))


(defun jabber-qim-muc-send-file (jc group filename)
  (interactive
   (jabber-muc-argument-list
    (list (read-file-name "Send File or Image: "))))
  (if (<= (nth 7 (file-attributes filename))
          jabber-qim-max-send-file-size)
      (jabber-qim-send-file filename jc group 'jabber-muc-send)
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
                          *jabber-qim-hostname*)))
    (when (or invited-members
              chat-with)
      (puthash muc-jid
               (append (when chat-with
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

(defun jabber-qim-readmark-message-p (message)
  (string= "readmark" (jabber-xml-get-attribute message 'type)))

(provide 'jabber-qim-extension)
