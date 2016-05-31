* Chinese-fonts-setup README                                         :README:

** 简介
Chinese-fonts-setup 是一个 emacs 中英文字体配置工具。可以比较方便地
实现中文字体和英文字体等宽（也就是大家常说的中英文对齐）。

注： 这个 package 特别适用于需要处理中英文混合表格的中文 org-mode 用户。
** 基本原理
Chinese-fonts-setup 的核心很简单，就是让中文字体和英文字体使用不同的字号，
从而实现中英文对齐，它和下面的样例代码原理是一样的：

#+BEGIN_EXAMPLE
(set-frame-font "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-14-*-*-*-*-0-iso10646-1")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "Microsoft Yahei" :size 16)))
#+END_EXAMPLE

** 使用特点
Chinese-fonts-setup 添加了许多辅助工具，使配置和调节字体和字号的工作更加简便快捷，
它有几个优点：

1. 安装即用：Chinese-fonts-setup 内置字体 fallback 功能，只需安装，就能够配置中文字体和英文字体，
   让中文可以 *正确* 显示（但未必完美），不会因为 emacs 配置中指定的字体不存在而报错。
2. 设置方便：Chinese-fonts-setup 自带一个 profile 文件编辑工具，可以让用户动态调整字体名称和字体
   大小，分分钟实现中文字体和英文字体的等宽对齐。

** 下载安装
1. 配置melpa源，参考：http://melpa.org/#/getting-started
2. M-x package-install RET chinese-fonts-setup RET
3. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：

#+BEGIN_EXAMPLE
(require 'chinese-fonts-setup)
#+END_EXAMPLE

** 配置使用
*** 编辑使用 profile
一个 profile 代表了一套字体配置，chinese-fonts-setup 使用 profile 的概念，
来维护多套字体配置，从而实现特定的环境使用特定的字体配置，
比如：在编程时使用 “Consolas + 微米黑”，在阅读文章时使用 “PragmataPro + 黑体”，等等。

在 `cfs-profiles-directory' 目录中, 每一个 profile 都对应一个 emacs-lisp 文件,
这些文件包含了英文字体设置，中文字体设置以及中文字体大小，其内容类似：

#+BEGIN_EXAMPLE
`cfs--custom-set-fontsnames' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *有效并可用* 的字体将被使用。
将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。另外，用户可以通过命令
`cfs-insert-fontname’ 来选择一个 *可用* 字体，然后在当前光标处插入其字体名称。
(setq cfs--custom-set-fontnames
      '(
        ("PragmataPro" "Monaco" "Consolas" "DejaVu Sans Mono" "Droid Sans Mono" "Courier" "Courier New" "Liberation Mono" "Ubuntu Mono" "Droid Sans Mono Pro" "Inconsolata" "Source Code Pro" "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter" "Panic Sans" "Hack" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Menlof" "Cousine" "Fira Mono" "Lekton" "M+ 1mn" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")
        ("文泉驿等宽微米黑" "Noto Sans S Chinese Regular" "微软雅黑" "Microsoft Yahei" "Microsoft_Yahei" "文泉驿等宽微米黑" "文泉驿等宽正黑" "黑体" "Hiragino Sans GB" "文泉驿正黑" "文泉驿点阵正黑" "SimHei" "SimSun" "NSimSun" "FangSong" "KaiTi" "FangSong_GB2312" "KaiTi_GB2312" "LiSu" "YouYuan" "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书" "STXihei" "STKaiti" "STSong" "STZhongsong" "STFangsong" "FZShuTi" "FZYaoti" "STCaiyun" "STHupo" "STLiti" "STXingkai" "STXinwei" "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋" "华文彩云" "华文新魏" "华文细黑" "华文行楷")
        ("HanaMinB")
        ))

`cfs--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B字体字号)
将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。
(setq cfs--custom-set-fontsizes
      '(
        (9    9.0 11.0)
        (10   10.5 12.5)
        (11.5 12.0 14.0)
        (12.5 13.5 15.0)
        (14   15.0 16.5)
        (16   16.5 20.0)
        (18   18.0 21.0)
        (20   21.0 24.0)
        (22   22.5 26.0)
        ))
#+END_EXAMPLE

Chinese-fonts-setup 默认使用三个 profile: profile1, profile2 和 profile3,
如果想使用其它有意义的名称，可以设置:

#+BEGIN_EXAMPLE
(setq cfs-profiles
    '("program" "org-mode" "read-book"))
#+END_EXAMPLE

用户可以使用下面两个命令快速切换 profile：

| Command            | Help                    |
|--------------------+-------------------------|
| cfs-switch-profile | 选择并切换 profile      |
| cfs-next-profile   | 直接切换到下一个profile |

如果用户觉得 *当前使用* 的 profile 不符合个人使用习惯，可以使用 `cfs-edit-profile'
命令来编辑当前 profile 文件（如果 profile 文件不存在，chinese-fonts-setup
会在编辑之前自动新建一个, *不需要用户手动创建 profile 文件* ），用户可以使用命令：
`cfs-regenerate-profile' 强制覆盖一个 *已经存在* 的 profile 文件！

`cfs-edit-profile' 命令会打开当前 profile 文件，并激活内置的 profile 编辑模式，
在编辑的过程中，用户可以使用下面三个命令 *快速* 的了解测试编辑效果：

| Key     | Command                         | Help                                   |
|---------+---------------------------------+----------------------------------------|
| C-c C-c | cfs-test-fontsizes-at-point     | 查看字体显示效果                       |
| C-up    | cfs-increment-fontsize-at-point | 增大光标下字号的大小，同时显示对齐效果 |
| C-down  | cfs-decrement-fontsize-at-point | 减小光标下字号的大小，同时显示对齐效果 |

配置完成后，有可能需要重启 Emacs。(参考： http://debbugs.gnu.org/db/17/1785.html)

[[./snapshots/cfs-edit-fontnames.gif]]

[[./snapshots/cfs-edit-fontsizes.gif]]

*** 调整字体大小
`chinese-fonts-setup' 使用下述两个命令调整字体大小:

| Command               | Help         |
|-----------------------+--------------|
| cfs-increase-fontsize | 增大字体大小 |
| cfs-decrease-fontsize | 减小字体大小 |

注意：在调整字体大小的同时，字号信息也会保存 ~/.emacs 中。

[[./snapshots/cfs-increase-and-decrease-fontsize.gif]]

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
