**基于jabber.el的emacs qim客户端**
===============================

**注意**：加入qtalk群需要执行jabber-qim-muc-join命令，原jabber-muc-join命令仅用于加入非qtalk群组

## **暂时不支持的Feature**

* 建群
* 发送图片/文件/表情

## **安装方法**

**依赖**： Linux操作系统，GNU Emacs 23.1以上（推荐用24+, 其他版本没测试过)，openssl，autoconf，automake，emacs-uuid(http://www.emacswiki.org/emacs/uuid.el)

运行：

    autoreconf -i
    ./configure
    make jabber-autoloads.el

然后，添加

    (load "jabber-autoloads")

到.emacs配置文件

## **参考配置**

    (add-to-list 'load-path "~/Documents/sources/qim-emacs") ; 本工程目录

    ; (setq jabber-qim-pubkey-file
    ;    "~/Documents/sources/qim-emacs/resources/qtalk_pub_key.pem") ; 公钥文件路径，默认为本工程目录下的resources/qtalk_pub_key.pem文件

    ; (setq jabber-qim-local-file-dir
    ;    "~/qim-local-files") ; qim保存收到的文件的目录（默认为本工程目录下的.cache目录）

    (load "jabber-autoloads")

    ; (setq jabber-debug-log-xml t)

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


    (setq jabber-qim-muc-autojoin
        '(("去哪儿度假bu@conference.ejabhost1"
            (:silence . t)))) ; :silence表示默认不在message框里提示消息，除非被@。需要提示的话就去掉这项设置

    ;; account list
    (setq jabber-account-list
    `(
        ("域用户名@ejabhost1" ; 例如：geng.li@ejabhost1
        (:network-server . "qt.corp.qunar.com")
        (:port . "5222")
        (:password . ,(jabber-qim-password "域用户名" "域密码")))))

    (jabber-connect-all)


## **引用代码**

emacs-web (https://github.com/nicferrier/emacs-web)

elisp-latch (https://github.com/skeeto/elisp-latch)

## **开发分支**

qim-emacs
