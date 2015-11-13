;;; Extensions for qim -*- lexical-binding: t -*-

(require 'jabber-core)

;;;###autoload
(defvar jabber-qim-ext-dir (format "%s%s"
                                   (file-name-directory
                                    (or load-file-name buffer-file-name))
                                   "jabber-qim-exts/"))

(add-to-list 'load-path
             jabber-qim-ext-dir)

;;;###autoload
(defvar jabber-qim-pubkey-file
  (format "%sresources/qtalk_pub_key.pem"
          jabber-qim-ext-dir))

;;;###autoload (autoload 'jabber-qim-password "jabber-qim-extension" "create qim password" t)
(defun jabber-qim-password (uid pwd)
  (require 'json)
  (shell-command-to-string
   (format "echo -n `echo '%s' | openssl  rsautl  -encrypt  -inkey %s  -pubin  | base64`"
           (json-encode `((:p . ,pwd)
                          (:a . "testapp")
                          (:u . ,uid)
                          (:d . ,(shell-command-to-string "echo -n `date '+%F %T'`"))))
           jabber-qim-pubkey-file)))


(require 'jabber-qim-env)
(require 'jabber-qim-webapi)
(require 'jabber-qim-chat)
(require 'jabber-qim-muc)


(add-to-list 'jabber-post-connect-hooks 'jabber-qim-user-muc-preload)

(add-to-list 'jabber-post-connect-hooks 'jabber-qim-users-preload)


(provide 'jabber-qim-extension)
