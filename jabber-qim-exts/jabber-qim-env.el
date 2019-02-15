;; jabber-qim-env.el QTalk extension for local system environment setup

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


(defvar *jabber-qim-version* "11001010")

(defvar jabber-qim-local-file-dir
  "~/qim-local-files")

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
  (format "%s/%s"
          *jabber-qim-resource-dir*
          "emotions"))

;;;###autoload
(defvar *jabber-qim-image-file-cache*
  (make-hash-table :test 'equal))


(provide 'jabber-qim-env)
