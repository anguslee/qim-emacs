;;; Extensions for qim -*- lexical-binding: t -*-

(require 'jabber-core)

;;;###autoload
(defvar jabber-qim-ext-dir (format "%s%s"
                                   (file-name-directory
                                    (or load-file-name buffer-file-name))
                                   "jabber-qim-exts/"))

(add-to-list 'load-path
             jabber-qim-ext-dir)


(require 'jabber-qim-env)
(require 'jabber-qim-webapi)
(require 'jabber-qim-chat)
(require 'jabber-qim-muc)


(add-to-list 'jabber-post-connect-hooks 'jabber-qim-user-muc-preload)

(add-to-list 'jabber-post-connect-hooks 'jabber-qim-users-preload)

(add-to-list 'jabber-post-connect-hooks #'(lambda (jc)
                                            (jabber-send-iq jc
                                                            nil
                                                            "get"
                                                            `(key ((xmlns . "urn:xmpp:key")))
                                                            #'(lambda (jc xml-data context)
                                                                (let ((key-node (jabber-xml-path xml-data '(("urn:xmpp:key" . "key")))))
                                                                  (plist-put
                                                                   (plist-get jc
                                                                              :state-data)
                                                                   :qim-auth-key (jabber-xml-get-attribute
                                                                                  key-node
                                                                                  'value))))
                                                            nil
                                                            'jabber-report-success "urn:xmpp:key")))


(provide 'jabber-qim-extension)
