;; Util functions for qim

(require 'json)
(require 'web)
(require 'latch)
(require 'jabber-util)
(require 'jabber-avatar)

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

(defun jabber-qim-api-request-post (callback command data mime-type)
  (web-json-post 
   callback
   :url (format "%s/%s" *jabber-qim-api-server* command)
   :data data
   :mime-type mime-type
   :json-object-type 'alist
   :json-array-type 'list))


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

;;;###autoload (autoload 'jabber-qim-user-vcard-jid "jabber-qim-util" "Return user jid" t)
(defun jabber-qim-user-vcard-jid (vcard)
  "Return user jid"
  (format "%s@ejabhost1" (decode-coding-string (cdr (assoc 'U vcard))
                                               'utf-8-emacs-unix)))

;;;###autoload (autoload 'jabber-qim-user-vcard-name "jabber-qim-util" "Return user name" t)
(defun jabber-qim-user-vcard-name (vcard)
  "Return user name"
  (decode-coding-string (cdr (assoc 'N vcard))
                        'utf-8-emacs-unix))

;;;###autoload (autoload 'jabber-qim-user-vcard-position "jabber-qim-util" "Return user position" t)
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

;;;###autoload (autoload 'jabber-qim-jid-nickname "jabber-qim-util" "Return user nickname" t)
(defun jabber-qim-jid-nickname (jid)
  "Return user nickname"
  (let ((user-vcard (gethash (jabber-jid-user jid)
                             *jabber-qim-user-vcard-cache*)))
    (when user-vcard
      (jabber-qim-user-vcard-name user-vcard))))


;;;###autoload
(defvar *jabber-qim-muc-vcard-cache*
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

;;;###autoload (autoload 'jabber-qim-get-muc-vcard "jabber-qim-util" "Return MUC vcard" t)
(defun jabber-qim-get-muc-vcard (muc-jid)
  "Return MUC vcard"
  (and
   (string-prefix-p "conference." (jabber-jid-server muc-jid))
   (or (gethash (jabber-jid-user muc-jid) *jabber-qim-muc-vcard-cache*)
       (lexical-let ((latch (make-one-time-latch))
                     (ret nil)
                     (vcard nil))
         (jabber-qim-api-request-post
          (lambda (data conn headers)
            (ignore-errors
              (setq vcard
                    (nth 0 (cdr (assoc 'data data)))))
            (setq ret t)
            (apply-partially #'nofify latch))
          "getmucvcard"
          (json-encode (vector `((:muc_name . ,(jabber-jid-user muc-jid))
                                 (:version . 0))))
          'applicaition/json)
         (wait latch 0.2)
         (if (null vcard)
             (puthash (jabber-jid-user muc-jid)
                      `((SN . ,(jabber-jid-user muc-jid))
                        (MN . ,muc-jid))
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


(defun jabber-qim-insert-file (file-desc face)
  "Insert file into chat buffer."
  (insert "\n")
  (insert (jabber-propertize
           (format "[File Received: %s] "
                   (cdr (assoc 'FileName
                               file-desc)))
           'face face))
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
  ;; (insert " ")
  ;; (insert-button "Forward To..."
  ;;                :file-decs file-desc
  ;;                'action #'(lambda (button)
  ;;                            (interactive )
  ;;                            (lexical-let* ((file-desc (button-get button :file-desc)))
  ;;                              )))
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
                      :image-object object-text
                      'action #'(lambda (button)
                                  (let* ((session-muc-alist (jabber-qim-session-muc-vcard-alist))
                                         (jid (jabber-read-jid-completing "Forward to:"
                                                                          (append (mapcar #'car session-muc-alist) *jabber-qim-user-jid-cache*)))
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
                                             (button-get button :image-object))
                                    )))
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

(defun jabber-qim-body-parse-file (body)
  (let ((file-desc (ignore-errors
                     (json-read-from-string body))))
    (when (and file-desc
             (cdr (assoc 'FileName file-desc))
             (cdr (assoc 'HttpUrl file-desc)))
        file-desc
        )))


(provide 'jabber-qim-util)
