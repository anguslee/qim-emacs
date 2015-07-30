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

(defvar *jabber-qim-image-server*
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
  (web-json-post 
   callback
   :url (format "%s/%s" *jabber-qim-api-server* command)
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
         (puthash (jabber-jid-user muc-jid) vcard *jabber-qim-muc-vcard-cache*)
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

(defun jabber-qim-insert-file (file-desc face)
  "Insert file into chat buffer."
  )


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
       (let ((image (jabber-qim-load-image value)))
         (if image
             (progn
               (insert "\n")
               (insert-image
                image
                value)
               (insert "\n"))
           (insert (jabber-propertize
                    value
                    'face face)))))
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
             (let ((file-path (format "%s/%s"
                                      (jabber-qim-local-images-cache-dir)
                                      (md5 body))))
               (unless (file-exists-p file-path)
                 (let ((coding-system-for-write 'binary))
                   (with-temp-file file-path
                     (insert body))))
               (setq image file-path))))
         (setq ret t)
         (apply-partially #'nofify latch))
     :url (format "%s/%s" *jabber-qim-image-server* url-path)
     )
    (wait latch 1.5)
    (when image
        (jabber-create-image image))
    ))

(defun jabber-qim-load-file (file-desc)
  (lexical-let ((file-path (format "%s/%s"
                           (jabber-qim-local-received-files-cache-dir)
                           (cdr (assoc 'FileName file-desc))))
        (url (format "%s/%s" *jabber-qim-image-server* (cdr (assoc 'HttpUrl file-desc)))))
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