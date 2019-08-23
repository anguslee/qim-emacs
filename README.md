**QTalk For Emacs, 扩展自[jabber.el](https://github.com/legoscia/emacs-jabber)**
===============================================================================


## **安装方法**

**依赖**：

* Linux或者Mac OSX系统安装GNU Emacs v24.5+, 低于这个版本的emacs可能会因为找不到subr-x模块报错。
* [wget命令](https://www.gnu.org/software/wget/)，chat buffer里加载图片需要；
* openssl
* autoconf, 推荐更新到最新版本，低版本可能在运行时报错。
* automake
* [emacs-uuid](http://www.emacswiki.org/emacs/uuid.el), 或者命令行能提供uuidgen命令；
* [ImageMagick](http://www.emacswiki.org/emacs/ImageMagick), 截屏功能依赖此。
* [dash.el](https://github.com/magnars/dash.el), 可以通过package-install安装

**安装运行**：


1. 编译

    首次：

    > autoreconf -i
    >
    > ./configure
    >
    > make jabber-autoloads.el

    每次更新代码需要重新运行上面的：

    > ./configure
    >
    > make jabber-autoloads.el

2. 初始化静态资源目录：
   * 目录下的资源内容请联系管理员获取，分别按照格式要求置入。资源目录结构说明与示例见: [jabber-qim-exts/resources](jabber-qim-exts/resources)
3. 添加上述静态资源目录到本地.emacs初始化配置：
   ```lisp
   (setq *jabber-qim-resource-dir*
       "<静态资源目录路径>")
   
   (setq *jabber-qim-pubkey-filename*
       "<用于客户端密码加密的公钥文件名，从静态资源目录的qim-auth-keys目录下选取>")
   ```
4. 根据服务端的导航服务，为本地.emacs添加以下初始化配置参数：
   ```lisp
   (setq *jabber-qim-http-url*
       "<服务端通用http接口url地址，取值baseaddress.httpurl>")

   (setq *jabber-qim-message-history-url*
       "<服务端历史消息接口url地址，取值baseaddress.javaurl>")")

   (setq *jabber-qim-file-server*
       "<服务端文件服务地址，取值baseaddress.fileurl>")

   (setq *jabber-qim-xmpp*
       "<xmpp服务器地址，取值baseaddress.xmpp>")

   (setq *jabber-qim-xmpp-port*
       "<xmpp服务端口号，取值baseaddress.xmppport>")

   (setq *jabber-qim-domain*
       "<用户id所属域名，取值baseaddress.domain>")
   ```
5. 设置load-path变量，本地文件保存目录，并加载初始化文件：
   ```lisp
   (add-to-list 'load-path "<本工程目录>")
   (setq jabber-qim-local-file-dir
        "~/qim-local-files") ; qim保存本地文件的目录，若不设置，默认为"~/qim-local-files"

   (load "jabber-autoloads")
   ```
6. 设置登录认证信息
   ```lisp
   (setq jabber-account-list
    `(
        (,(format "%s@%s" "<用户ID>" *jabber-qim-domain*)
        (:network-server . ,*jabber-qim-xmpp*)
        (:port . ,*jabber-qim-xmpp-port*)
        (:password . ,(jabber-qim-password "<用户ID>" "<登录密码>")))))
   ```
7. 登录

    *M-x jabber-connect-all (C-x C-j C-c)*
   

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

6. 以其他客户端可以直接点击打开的格式发送链接：

    *M-x jabber-qim-send-link (C-x C-j C-i)*


7. 邀请入群组：

    *M-x jabber-muc-invite (C-x C-j C-v)*

8. 聊天状态下打开功能菜单：

    *C-c C-c* （点对点聊天菜单）

    *C-c [return]* （群聊菜单）

9. 开启/关闭群组消息提示（重启后仍生效）

    *M-x jabber-muc-toggle-message-alert*

10. 切换到含未读消息的会话

    *M-x jabber-qim-chat-switch-to-unread (C-x C-j C-u)* （可通过补全方式选择含未读消息的会话）

    *M-x jabber-activity-switch-to (C-x C-j C-l)* （不经选择直接跳到最近未读消息的会话）

需要输入群组ID或者用户ID的时候，都可以用TAB在minibuffer里做补全。用户ID可以用域用户名或者姓名的方式做前缀检索。


## **受限Feature与已知问题**

### 2018-02-27

* GNU Emacs 25.3版本，初始化群聊时会因为访问服务端api报错失败，因此目前建议用24.x版本
* 不能发送表情
* 发送文件和图片的大小不能超过10MB
* 接收到大尺寸的图片时，emacs进程会卡顿
* buffer里不能播放gif动图
* 不支持点对点加密聊天


## **引用资源**

* [emacs-web](https://github.com/nicferrier/emacs-web) 用于与后端api服务做交互，为易用性做了小的修改。
* [s.el](https://github.com/magnars/s.el)

## **分支说明**

* 开发分支：qim-emacs
* RELEASE分支：master
