;; Auth for qim

;;;###autoload
(defvar jabber-qim-pubkey-file
  (format "%sresources/qtalk_pub_key.pem"
          (file-name-directory
           (or load-file-name buffer-file-name))))

;;;###autoload (autoload 'jabber-qim-password "jabber-qim-auth" "create qim password" t)
(defun jabber-qim-password (uid pwd)
  (require 'json)
  (shell-command-to-string
   (format "echo -n `echo '%s' | openssl  rsautl  -encrypt  -inkey %s  -pubin  | base64`"
           (json-encode `((:p . ,pwd)
                          (:a . "testapp")
                          (:u . ,uid)
                          (:d . ,(shell-command-to-string "echo -n `date '+%F %T'`"))))
           jabber-qim-pubkey-file)))

(provide 'jabber-qim-auth)
