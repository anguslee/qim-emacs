;;; Extensions for qim -*- lexical-binding: t -*-

;; jabber-qim-extension.el QTalk extension entry

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

(defvar *jabber-qim-timers*
  '())

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
                                                                                  'value)))
                                                                (add-to-list '*jabber-qim-timers*
                                                                             (run-with-timer 0 jabber-qim-user-vcard-reload-cycle 'jabber-qim-users-preload jc))
                                                                (jabber-qim-user-muc-preload jc))
                                                            nil
                                                            'jabber-report-success "urn:xmpp:key")))

(defun jabber-qim-cancel-timers ()
  (mapcar #'cancel-timer
          *jabber-qim-timers*)
  (setq *jabber-qim-timers* nil)
  t)

(add-hook 'jabber-post-disconnect-hook
          'jabber-qim-cancel-timers)

(provide 'jabber-qim-extension)
