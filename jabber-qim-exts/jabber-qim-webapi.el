;;; Extensions for qim web APIs -*- lexical-binding: t -*-

(require 'web)

;;;###autoload
(defvar *jabber-qim-api-server*)

;;;###autoload
(defvar *jabber-qim-file-server*)

;;;###autoload
(defvar *jabber-qim-file-service-version*
  "v2")

(add-to-list 'web-json-expected-mimetypes-list
             "text/json")

(cl-defun web-json-get (callback
                        &key
                        url headers
                        (logging t)
                        (json-array-type json-array-type)
                        (json-object-type json-object-type)
                        (json-key-type json-key-type)
                        (expectation-failure-callback
                         'web-json-default-expectation-failure))
  "GET URL expecting a JSON response sent to CALLBACK. Similar to
`web-json-post' defined in web.el.

See `web-json-expected-mimetypes-list' for the list of Mime Types
we accept JSON for.  This may be let bound.  If the expectation
is not met then EXPECTATION-FAILURE-CALLBACK is called being
passed the CALLBACK parameters.  By default
EXPECTATION-FAILURE-CALLBACK is
`web-json-default-expectation-failure'.

The CALLBACK is called as:

  CALLBACK RESPONSE-DATA HTTPCON RESPONSE-HEADER

so the function may be defined like this:

  (lambda (data &rest stuff) ...)

HEADERS may be specified, these are treated as extra-headers to
be sent with the request.

JSON-ARRAY-TYPE, JSON-OBJECT-TYPE and JSON-KEY-TYPE, if present,
are used to let bind the `json-read' variables of the same name
affecting the resulting lisp structure."
  (let ((closed-json-array-type json-array-type)
        (closed-json-object-type json-object-type)
        (closed-json-key-type json-key-type))
    (web-http-get
     (lambda (httpcon header http-data)
       ;; Add a member test for the MIMETYPE expectation
       (let ((lisp-data
              (condition-case err
                  (web/json-parse
                   http-data
                   :json-array-type closed-json-array-type
                   :json-object-type closed-json-object-type
                   :json-key-type closed-json-key-type)
                (error
                 (when logging
                   (message "web-json-get expectation failure %S" err))
                 (funcall expectation-failure-callback
                          http-data httpcon header)))))
         (funcall callback lisp-data httpcon header)))
      :url url
      :extra-headers headers
      :logging logging)))


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


(defun jabber-qim-api-request-get (callback command &optional auth-info)
  (web-json-get 
      callback
      :url (format "%s/%s?u=%s&k=%s" *jabber-qim-api-server*
                   command
                   (or (cdr (assoc :u auth-info)) "")
                   (or (cdr (assoc :k auth-info)) ""))
      :json-object-type 'alist
      :json-array-type 'list))

(provide 'jabber-qim-webapi)
