This library configures Magit and Evil to play well with each
other. For some background see https://github.com/magit/evil-magit/issues/1.

Installation and Use
====================

Everything is contained in evil-magit.el, so you may download and load that file
directly. The recommended method is to use MELPA via package.el (`M-x
package-install RET evil-magit RET`).

Evil and Magit are both required. After requiring those packages, the following
will setup the new key bindings for you.

optional: this is the evil state that evil-magit will use
(setq evil-magit-state 'motion)
(require 'evil-magit)

Use `evil-magit-revert` to revert changes made by evil-magit to the default
evil+magit behavior.

   |                      |         | Evil-magit                  | Evil-magit                 |
   | Category             | Default | without yank option         | with yank option (default) |
   |----------------------+---------+-----------------------------+----------------------------|
   | cherry pick          | =a=A=   |                             |                            |
   | branch               | =b=     |                             |                            |
   | bisect               | =B=     |                             |                            |
   | commit               | =c=     |                             |                            |
   | diff                 | =d/D=   |                             |                            |
   | ediff                | =e/E=   |                             |                            |
   | fetch                | =f=     |                             |                            |
   | pull                 | =F=     |                             |                            |
   | refresh              | =g=     | =gr/gR=                     |                            |
   | help                 | =h/?=   |                             |                            |
   | ignore               | =i/I=   |                             |                            |
   | jump                 | =j=     | =g=                         |                            |
   | delete               | =k=     | =x=                         |                            |
   | untrack              | =K=     | =X=                         |                            |
   | log                  | =l/L=   |                             |                            |
   | merge                | =m=     |                             |                            |
   | remote               | =M=     |                             |                            |
   | next section         | =n=     | =C-j=                       |                            |
   | next section sibling | =M-n=   | =gj= or =]=                 |                            |
   | submodule            | =o=     | =>=                         |                            |
   | prev section         | =p=     | =C-k=                       |                            |
   | prev section sibling | =M-p=   | =gk= or =[=                 |                            |
   | push                 | =P=     | =P= or =p=                  |                            |
   | quit                 | =q=     | =q= or =ESC=                |                            |
   | rebase               | =r=     |                             |                            |
   | rename               | =R=     |                             |                            |
   | stage                | =s/S=   |                             |                            |
   | tag                  | =t=     |                             |                            |
   | notes                | =T=     |                             |                            |
   | unstage              | =u/U=   |                             |                            |
   | revert               | =v/V=   | =o/O=                       |                            |
   | am                   | =w=     |                             |                            |
   | patch                | =W=     |                             |                            |
   | reset                | =x=     | =C-r= (=X= in branch popup) |                            |
   | show-refs            | =y=     |                             | =yr= (=y= in popup)        |
   | cherry               | =Y=     |                             |                            |
   | stash                | =z/Z=   |                             |                            |
   | git-cmd              | =:=     | =¦=                         |                            |
   | run                  | =!=     |                             |                            |
   | copy section info    | =C-w=   |                             | =ys=                       |
   | copy buffer info     | =M-w=   |                             | =yb=                       |

* New Commands

   |                           | Evil-magit               | Evil-magit                 |
   | Command                   | without yank option      | with yank option (default) |
   |---------------------------+--------------------------+----------------------------|
   | evil-goto-line            | =G=                      |                            |
   | evil-next-visual-line     | =j=                      |                            |
   | evil-previous-visual-line | =k=                      |                            |
   | evil-search-next          | =n=                      |                            |
   | evil-search-previous      | =N=                      |                            |
   | set-mark-command          | =v= or =V=               | =C-SPC=                    |
   | evil-visual-line          | under =M-x=              | =v= or =V=                 |
   | evil-ex                   | =:=                      |                            |
   | evil-search-forward       | =/=                      |                            |
   | evil-scroll-page-up       | =C-b=                    |                            |
   | evil-scroll-down          | =C-d=                    |                            |
   | evil-scroll-page-down     | =C-f=                    |                            |
   | evil-scroll-up            | =C-u= (if =C-u= scrolls) |                            |
   | evil-emacs-state          | =C-z=                    |                            |
   | evil-yank-line            | under =M-x=              | =yy=                       |

Any other bindings are meant to be consistent with these.

Disclaimer
==========

Given the complexity of magit key bindings combined with the complexity of git
itself, it is possible that there are some rough edges where the current binding
is not the expected one in a buffer. It will be very helpful for you to report
any such instances.
