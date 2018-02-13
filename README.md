**基于jabber.el的emacs qim客户端**
===============================


## **安装方法**

**依赖**：

* Linux或者Mac OSX系统安装GNU Emacs v24.5+, 低于这个版本的emacs可能会因为找不到subr-x模块报错。其他组合没测试过；
* [wget命令](https://www.gnu.org/software/wget/)，chat buffer里加载图片需要；
* openssl
* autoconf
* automake
* [emacs-uuid](http://www.emacswiki.org/emacs/uuid.el), 或者命令行能提供uuidgen命令；
* [ImageMagick](http://www.emacswiki.org/emacs/ImageMagick), 截屏功能依赖此。
* [dash.el](https://github.com/magnars/dash.el), 可以通过package-install安装

**安装运行**：


1. 编译运行
```
    autoreconf -i
    /configure
    make jabber-autoloads.el
```
每次更新代码需要重新运行上面的：
```
    ./configure
    make jabber-autoloads.el
```
2. 初始化静态资源目录，目录下包含：
  * emoticons 静态表情包目录
  * qim-auth-keys 登录验证加密用公钥目录
3. 添加上述静态资源目录到本地.emacs初始化配置：
```lisp
   (setq *jabber-qim-resource-dir*
   "<静态资源目录路径>")
   (setq *jabber-qim-pubkey-filename*
   "<用于客户端密码加密的公钥文件名，从静态资源目录的qim-auth-keys目录下选取>")
```
4. 根据服务端的导航服务，为本地.emacs添加以下初始化配置参数：
```lisp
   (setq *jabber-qim-api-server*
   "<服务端接口API地址，取值baseaddress.apiurl>")

    (setq *jabber-qim-file-server*
    "<服务端文件服务地址，取值baseaddress.fileurl>")

    (setq *jabber-qim-xmpp*
    "<xmpp服务器地址，取值baseaddress.xmpp>")

    (setq *jabber-qim-xmpp-port*
    "<xmpp服务端口号，取值baseaddress.xmppport>")

    (setq *jabber-qim-domain*
    "<用户id所属域名，取值baseaddress.domain>")
```
5. 设置load-path变量并加载初始化文件：
```lisp
   (add-to-list 'load-path "<本工程目录>")
   (load "jabber-autoloads")
```

## **配置方法示例**

```lisp
    (add-to-list 'load-path "~/Documents/sources/qim-emacs") ; 本工程目录

    ; (setq jabber-qim-pubkey-file
    ;    "~/Documents/sources/qim-emacs/resources/qtalk_pub_key.pem") ; 公钥文件路径，默认为本工程目录下的jabber-qim-exts/resources/qtalk_pub_key.pem文件

    ; (setq jabber-qim-local-file-dir
    ;    "~/qim-local-files") ; qim保存收到的文件的目录（默认为本工程目录下的jabber-qim-exts/.cache目录）

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
```
    
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

9. 切换到含未读消息的会话

    *M-x jabber-qim-chat-switch-to-unread (C-x C-j C-u)*

需要输入群组ID或者用户ID的时候，都可以用TAB在minibuffer里做补全。用户ID可以用域用户名或者姓名的方式做前缀检索。


## **受限Feature**

* 不能发送表情
* 发送文件和图片的大小不能超过10MB
* buffer里不能播放gif动图
* 不支持点对点加密聊天


## **开发标准参照**

* [客户端 MessageType 值设定](http://wiki.corp.qunar.com/confluence/pages/viewpage.action?pageId=105916988)
* [Message消息属性值详解](http://wiki.corp.qunar.com/confluence/pages/viewpage.action?pageId=159685687)
* [QTalk&QChat文件和图片上传下载](http://wiki.corp.qunar.com/confluence/pages/viewpage.action?pageId=98573995)
* QTalk导航服务: (https://qt.qunar.com/package/static/qtalk/nav)

## **引用资源**

* [emacs-web](https://github.com/nicferrier/emacs-web)
* [s.el](https://github.com/magnars/s.el)

## **分支说明**

* 开发分支：qim-emacs

