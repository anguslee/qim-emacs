**基于jabber.el的emacs qim客户端**
===============================

**注意**：加入qtalk群需要执行jabber-qim-muc-join命令，原jabber-muc-join命令仅用于加入非qtalk群组

Bug反馈：geng.li@qunar.com

## **常用功能**

1. 开启点对点聊天：

    M-x jabber-chat-with

2. 加入群组：

    M-x jabber-qim-muc-join

3. 创建群组：

    M-x jabber-qim-chat-start-groupchat

4. 发送文件：

    M-x jabber-qim-muc-send-file （要求当前buffer是群聊）
    M-x jabber-qim-chat-send-file （要求当前buffer是点对点聊天）

5. 聊天状态下打开功能菜单：

    C-c C-c （点对点聊天菜单）
    C-c [return] （群聊菜单）

需要输入群组ID或者用户ID的时候，都可以用TAB在minibuffer里做补全。用户ID只能用域用户名的方式做前缀检索。


## **受限Feature**

* 不能发送表情
* 发送文件和图片的大小不能超过10MB

## **安装方法**

**依赖**： Linux操作系统，GNU Emacs推荐用v24+, 其他版本没测试过，openssl，autoconf，automake，emacs-uuid(http://www.emacswiki.org/emacs/uuid.el)

运行：

    git clone git@gitlab.corp.qunar.com:geng.li/qim-emacs.git
    cd qim-emacs
    autoreconf -i
    ./configure
    make jabber-autoloads.el

然后，添加

    (load "jabber-autoloads")

到.emacs配置文件

每次更新代码需要重新运行上面的：

    make jabber-autoloads.el

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
