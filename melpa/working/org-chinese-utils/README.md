- [介绍](#介绍)
  - [安装](#安装)
  - [使用](#使用)

# 介绍<a id="orgb8c8e57"></a>

org-chinese-utils 包含了以下工具，可以方便 org-mode 中文用户：

1.  将 org 导出为 HTML 时删除不必要的空格。
2.  按 'C-c C-c', 根据当前内容智能折行。
3.  如果 org-babel 结果中包含表格时，对表格进行对齐处理。

## 安装<a id="orgdb8e78e"></a>

org-chinese-utils is now available from the famous emacs package repo
[melpa](http://melpa.milkbox.net/), so the recommended way is to install it
through emacs package management system.

## 使用<a id="org5d19128"></a>

    (require 'org)
    (require 'ox)
    (require 'org-chinese-utils)
    (org-chinese-utils-enable)
