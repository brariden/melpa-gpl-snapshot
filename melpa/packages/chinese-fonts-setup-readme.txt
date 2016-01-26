* Chinese-fonts-setup 简要说明                                       :README:
`chinese-fonts-setup' 是一个emacs中文字体配置工具。可以比较方便地
实现中文字体和英文字体等宽（也就是大家常说的中英文对齐）。

这个package特别适用于需要处理中英文混合表格的中文org-mode用户。

** 过程展示
http://www.tudou.com/programs/view/v7Kr0_a9INw/

** 下载
https://github.com/tumashu/chinese-fonts-setup

** 安装
1. 配置melpa源，参考：http://melpa.org/#/getting-started
2. M-x package-install RET chinese-fonts-setup RET
3. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：

#+BEGIN_EXAMPLE
(require 'chinese-fonts-setup)
#+END_EXAMPLE

** 配置
chinese-fonts-setup 使用profile的概念，来实现特定的环境使用特定的
字体配置，比如：在编程时使用 “Consolas + 微米黑”，在阅读文章时使用
“PragmataPro + 黑体”。

每一个profile都是一个emacs-lisp文件。其中包括了英文字体设置，中文字体设置
以及中文字体大小。

chinese-fonts-setup 默认使用三个profile: profile1, profile2 和 profile3,
如果想使用其他有意义的名称，可以使用下面类似的方式配置:

#+BEGIN_EXAMPLE
(setq cfs-profiles
    '("program" "org-mode" "read-book"))
#+END_EXAMPLE

所有的profile文件都保存在 `cfs-profiles-directory' 对应的目录中。
如果profile文件不存在， `chinese-fonts-setup' 将使用其自带的fallback
信息来配置字体。

在运行profile编辑命令`cfs-edit-profile'的时候，缺失的falback文件
将会自动创建，其原始内容为软件包自带的fallback信息。

`chinese-fonts-setup' 默认不会覆盖已经存在的profile文件。当需要重置
某个profile文件时，可以使用命令：`cfs-regenerate-profile'。这个命令
会强制覆盖profile文件，请做好备份。

切换 profile 的命令有：

| Command            | Help                    |
|--------------------+-------------------------|
| cfs-switch-profile | 使用ido切换profile      |
| cfs-next-profile   | 直接切换到下一个profile |

如果当前的profile不适合时，可以通过`cfs-edit-profile'来编辑当前
的profile文件。chinese-fonts-setup自带一个profile-edit编辑模式。

| Key     | Command                         | Help                                   |
|---------+---------------------------------+----------------------------------------|
| C-c C-c | cfs-test-fontsizes-at-point     | 查看字体显示效果                       |
| C-up    | cfs-increment-fontsize-at-point | 增大光标下字号的大小，同时显示对齐效果 |
| C-down  | cfs-decrement-fontsize-at-point | 减小光标下字号的大小，同时显示对齐效果 |

配置完成后，有可能需要重启 Emacs。(参考： http://debbugs.gnu.org/db/17/1785.html)

** 调整字体大小
`chinese-fonts-setup' 使用下述两个命令调整字体大小:

| Command               | Help         |
|-----------------------+--------------|
| cfs-increase-fontsize | 增大字体大小 |
| cfs-decrease-fontsize | 减小字体大小 |

在调整字体大小的同时，字号信息也通过customize-save-variable函数保存到~/.emacs中了。

** Tips

1. 使用命令: `describe-char' 可以了解光标处字符使用什么字体。
2. 在 scratch 中写一行 elisp 代码： (cl-prettyprint (font-family-list)),
   执行后，就会在 scratch 中插入当前可用字体的名称列表，这是一个很有用的技巧。
3. 命令：`cfs-insert-fontname', 可以让用户选择一个可用字体插入到当前光标处。
4. Windows 用户 (特别是 Windows XP 用户) 可以安装 MacType 软件来优化
   字体显示效果，推荐使用。
5. Mac 用户配置 profile 文件的时候，偶尔会遇到 'C-c C-c' 刷新缓慢的问题，这可能
   是 ext-b 字体缺失引起的，建议安装 ext-b 字体试试。
   1. Ext-B字符列表: https://cdo.wikipedia.org/wiki/Wikipedia:Unicode%E6%93%B4%E5%B1%95%E6%BC%A2%E5%AD%97
   2. HanaMinB 下载地址: https://osdn.jp/projects/hanazono-font/downloads/62072/hanazono-20141012.zip/

** 参考文章
1. http://baohaojun.github.io/perfect-emacs-chinese-font.html
2. http://zhuoqiang.me/torture-emacs.html
