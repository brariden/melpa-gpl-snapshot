# Introduction

`zenity-color-picker.el` uses
[Zenity](https://help.gnome.org/users/zenity/stable/) to enhance Emacs' with a
graphical color picker.

# Usage

The main functionality of the package can be accessed through the
`zenity-cp-color-at-point-dwim` function. The function either adjusts the color
at point or insert a new color in case the point is not over a color code. Since
the name is quite a mouthful, you might want to consider binding it to a key
combination. For example:

	(global-set-key (kbd "C-c e c") #'zenity-cp-color-at-point-dwim)


