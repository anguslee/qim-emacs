**基于jabber.el的emacs qim客户端**
===============================

![界面效果图](http://wiki.corp.qunar.com/download/attachments/69142038/demo-screen.png "界面效果")


**注意**：加入qtalk群需要执行jabber-qim-muc-join命令，原jabber-muc-join命令仅用于加入非qtalk群组

## **技术支持**

讨论组：Emacs版qtalk讨论组 (群id：5b241f5d37aefff23b8a8fd9b721f8fb@conference.ejabhost1)

已知问题列表: http://gitlab.corp.qunar.com/geng.li/qim-emacs/issues

## **常用功能**

1. 开启点对点聊天：

    *M-x jabber-chat-with (C-x C-j C-j)*

2. 加入群组：

    *M-x jabber-qim-muc-join (C-x C-j C-m)*

3. 创建群组：

    *M-x jabber-qim-chat-start-groupchat (C-x C-j C-g)*

4. 发送文件：

    *M-x jabber-qim-send-file (C-x C-j C-f)*

5. 截屏：

    *M-x jabber-qim-send-screenshot (C-x C-j C-s)*

6. 邀请入群组：

    *M-x jabber-muc-invite (C-x C-j C-v)*

7. 聊天状态下打开功能菜单：

    *C-c C-c* （点对点聊天菜单）

    *C-c [return]* （群聊菜单）

8. 开启/关闭群组消息提示（重启后仍生效）

    *M-x jabber-muc-toggle-message-alert*

需要输入群组ID或者用户ID的时候，都可以用TAB在minibuffer里做补全。用户ID可以用域用户名或者姓名的方式做前缀检索。


## **受限Feature**

* 不能发送表情
* 发送文件和图片的大小不能超过10MB
* buffer里不能播放gif动图

## **安装方法**

**依赖**：

* Linux或者Mac OSX系统安装GNU Emacs v24.5+, 低于这个版本的emacs可能会因为找不到subr-x模块报错。其他组合没测试过；
* [wget命令](https://www.gnu.org/software/wget/)，chat buffer里加载图片需要；
* openssl
* autoconf
* automake
* [emacs-uuid](http://www.emacswiki.org/emacs/uuid.el), 或者命令行能提供uuidgen命令；
* [ImageMagick](http://www.emacswiki.org/emacs/ImageMagick), 截屏功能依赖此。

**安装运行**：

    git clone git@gitlab.corp.qunar.com:geng.li/qim-emacs.git
    cd qim-emacs
    autoreconf -i
    ./configure
    make jabber-autoloads.el

然后，添加

    (load "jabber-autoloads")

到.emacs配置文件

每次更新代码需要重新运行上面的：

    ./configure
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


    ;; account list
    (setq jabber-account-list
    `(
        ("域用户名@ejabhost1" ; 例如：geng.li@ejabhost1
        (:network-server . "qt.corp.qunar.com")
        (:port . "5222")
        (:password . ,(jabber-qim-password "域用户名" "域密码")))))

    (jabber-connect-all)


## **引用资源**

* [emacs-web](https://github.com/nicferrier/emacs-web)
* [elisp-latch](https://github.com/skeeto/elisp-latch)
* Emoji-One表情来自[Emoji One](http://emojione.com/ "Emoji art supplied by Emoji One")

## **分支说明**

* 开发分支：qim-emacs

