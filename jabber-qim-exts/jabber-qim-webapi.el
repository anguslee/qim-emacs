;;; Extensions for qim web APIs -*- lexical-binding: t -*-

(require 'web)

(defvar *jabber-qim-api-server*
  "https://qtapi.corp.qunar.com")

(defvar *jabber-qim-file-server*
  "https://qtalk.corp.qunar.com")

(defvar *jabber-qim-file-service-version* "v2")

(add-to-list 'web-json-expected-mimetypes-list
             "text/json")

(defun jabber-qim-api-request-post (callback command data mime-type &optional auth-info)
  (web-json-post 
   callback
   :url (format "%s/%s?u=%s&k=%s" *jabber-qim-api-server*
                command
                (or (cdr (assoc :u auth-info)) "")
                (or (cdr (assoc :k auth-info)) ""))
   :data data
   :mime-type mime-type
   :json-object-type 'alist
   :json-array-type 'list))

(provide 'jabber-qim-webapi)
