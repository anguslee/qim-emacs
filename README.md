**基于jabber.el的emacs qim客户端**
===============================


#### **开发分支**
qim-emacs

#### **安装方法**
Make sure that autoconf and automake are installed and run

"autoreconf -i".
./configure
make

You can specify which emacs you want to use:
./configure EMACS=emacs-or-xemacs-21.4

You can also install jabber.el by hand.  Put all .el files somewhere
in your load-path, or have your load-path include the directory
they're in.  To install the Info documentation, copy jabber.info to
/usr/local/info and run "install-info /usr/local/info/jabber.info".

After installation by either method, add (load "jabber-autoloads") to
your .emacs file.


#### **参考配置**

    (add-to-list 'load-path "~/Documents/sources/emacs-jabber")

    (setq qtalk-pubkey-file
        "~/Documents/sources/emacs-jabber/resources/qtalk_pub_key.pem")

    (load "jabber-autoloads")

    (setq jabber-debug-log-xml t)

    (setq jabber-invalid-certificate-servers '("qt.corp.qunar.com"))

    (setq starttls-extra-arguments  '("--insecure"))
    (setq jabber-history-enabled t)
    (setq jabber-use-global-history nil)
    (setq jabber-history-muc-enabled t)
    (setq jabber-history-dir "~/qtalk-logs")
    (setq jabber-muc-colorize-foreign t) ;; nick color


    (setq jabber-alert-presence-message-function
        (lambda (WHO OLDSTATUS NEWSTATUS STATUSTEXT)
        nil))


    (defun format-qtalk-password (uid password)
        `((:p . ,password)
        (:a . "testapp")
        (:u . ,uid)
        (:d . ,(shell-command-to-string "echo -n `date '+%F %T'`"))))


    (defun qtalk-pwd (uid pwd)
        (require 'json)
        (shell-command-to-string
        (format "echo -n `echo '%s' | openssl  rsautl  -encrypt  -inkey %s  -pubin  | base64`"
        (json-encode (format-qtalk-password uid pwd))
        qtalk-pubkey-file)))


    (setq jabber-muc-autojoin
        '("qtalk客户端开发群@conference.ejabhost1"))

    ;; account list
    (setq jabber-account-list
    `(
        ("geng.li@ejabhost1"
        (:network-server . "qt.corp.qunar.com")
        (:port . "5222")
        (:password . ,(qtalk-pwd "RTX用户名" "你的密码")))))

    (jabber-connect-all)




