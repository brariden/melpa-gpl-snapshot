Make ediff a little evil. This configures ediff to be a little more friendly
users of vim-like keybindings. Consult the help buffer (=?=) for more info.

Here's a table describing the bindings

| Command                     | Original Binding | Evil-ediff  |
|-----------------------------+------------------+-------------|
| ediff-jump-to-difference    | j                | d           |
| ediff-previous-difference   | p,DEL            | C-k,p,DEL   |
| ediff-next-difference       | n,SPC            | C-j,n,SPC   |
| jump to first difference    | 1j               | gg (or 1d)  |
| jump to last difference     | N/A              | G           |
| ediff-next-difference       | n,SPC            | C-j,n,SPC   |
| scroll down 1 line          | C-u 1 v          | j           |
| scroll up 1 line            | C-u 1 V          | k           |
| scroll down half page       | v,C-v            | C-d,v,C-v   |
| scroll up half page         | V,M-v            | C-u,V,M-v   |
| ediff-suspend               | z                | C-z         |
| scroll left                 | >                | zh          |
| scroll right                | <                | zl          |
