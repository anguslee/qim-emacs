;; Qim local system environment

(defvar jabber-qim-local-file-dir
  (format "%s.cache"
          (file-name-directory
           (or load-file-name buffer-file-name))))

(defvar *jabber-qim-hostname*
  "ejabhost1")

(defvar *jabber-qim-muc-sub-hostname*
  "conference")


(defun jabber-qim-local-images-cache-dir ()
  (format "%s/images" (expand-file-name jabber-qim-local-file-dir)))

(defun jabber-qim-local-received-files-cache-dir ()
  (format "%s/received-files" (expand-file-name jabber-qim-local-file-dir)))

(defun jabber-qim-local-screenshots-dir ()
  (format "%s/screenshots" (expand-file-name jabber-qim-local-file-dir)))


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
