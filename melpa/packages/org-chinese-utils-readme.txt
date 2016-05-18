* 介绍                                                             :README:
org-chinese-utils 包含了以下工具，可以方便 org-mode 中文用户：
1. 将 org 导出为 HTML 时删除不必要的空格。
2. 按 'C-c C-c', 根据当前内容智能折行。
3. 如果 org-babel 结果中包含表格时，对表格进行对齐处理。

** 安装
org-chinese-utils is now available from the famous emacs package repo
[[http://melpa.milkbox.net/][melpa]], so the recommended way is to install it
through emacs package management system.

** 使用
#+BEGIN_EXAMPLE
(require 'org)
(require 'ox)
(require 'org-chinese-utils)
(org-chinese-utils-enable)
#+END_EXAMPLE
