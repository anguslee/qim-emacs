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
                     (ret nil))
         (jabber-qim-api-request-post
          (lambda (data conn headers)
            (setq ret
                  (nth 0 (cdr (assoc 'data data))))
            (apply-partially #'nofify latch))
          "getmucvcard"
          (json-encode (vector `((:muc_name . ,(jabber-jid-user muc-jid))
                                 (:version . 0))))
          'applicaition/json)
         (wait latch 0.2)
         (puthash (jabber-jid-user muc-jid) ret *jabber-qim-muc-vcard-cache*)
         ret))))

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
             (progn
               (insert "\n")
               (insert-image
                image
                value)
               (insert "\n"))
           (insert (jabber-propertize
                    value
                    'face face)))))
      ('image
       (let ((image (jabber-qim-load-image value)))
         (if image
             (insert-image
              image
              value)
           (insert (jabber-propertize
                    value
                    'face face)))))
      ('url
       (insert (jabber-propertize
                (format " %s " value)
                'face face)))
      (t
       (insert (jabber-propertize
                object-text
                'face face))))))

(defun jabber-qim-load-image (url-path)
  (lexical-let ((latch (make-one-time-latch))
                (ret nil))
    (web-http-get
     #'(lambda (httpc header body)
         (let* ((img-fp (md5 body))
                (file-path (format "%s/%s"
                                   (jabber-qim-local-images-cache-dir)
                                   img-fp)))
           (unless (file-exists-p file-path)
             (let ((coding-system-for-write 'binary))
               (with-temp-file file-path
                 (insert body))))
           (setq ret file-path))
         (apply-partially #'nofify latch))
     :url (format "%s/%s" *jabber-qim-image-server* url-path)
     )
    (wait latch 1.5)
    (if ret
        (create-image ret)
      nil)
    ))

(provide 'jabber-qim-util)
