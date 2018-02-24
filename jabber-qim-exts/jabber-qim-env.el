;; Qim local system environment

(defvar jabber-qim-local-file-dir
  (format "%s.cache"
          (file-name-directory
           (or load-file-name buffer-file-name))))

(defvar *jabber-qim-resource-dir*
  (format "%sresources" jabber-qim-ext-dir))
   
(defvar *jabber-qim-pubkey-filename*
  "pub_key_chat_release")


;;;###autoload (autoload 'jabber-qim-password "jabber-qim-extension" "create qim password" t)
(defun jabber-qim-password (uid pwd)
  (require 'json)
  (shell-command-to-string
   (format "echo -n `echo '%s' | openssl  rsautl  -encrypt  -inkey %s  -pubin  | base64`"
           (json-encode `((:p . ,pwd)
                          (:a . "testapp")
                          (:u . ,uid)
                          (:d . ,(shell-command-to-string "echo -n `date '+%F %T'`"))))
           (format "%s/qim-auth-keys/%s"
                   *jabber-qim-resource-dir*
                   *jabber-qim-pubkey-filename*))))




;;;###autoload
(defvar *jabber-qim-domain*)

(defvar *jabber-qim-muc-sub-hostname*
  "conference")


(defun jabber-qim-local-images-cache-dir ()
  (format "%s/images" (expand-file-name jabber-qim-local-file-dir)))

(defun jabber-qim-local-received-files-cache-dir ()
  (format "%s/received-files" (expand-file-name jabber-qim-local-file-dir)))

(defun jabber-qim-local-screenshots-dir ()
  (format "%s/screenshots" (expand-file-name jabber-qim-local-file-dir)))

(require 'dired-aux)
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

;;;###autoload
(defvar *jabber-qim-image-file-cache*
  (make-hash-table :test 'equal))


(provide 'jabber-qim-env)
