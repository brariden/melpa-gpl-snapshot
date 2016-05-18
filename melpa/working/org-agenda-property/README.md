Propertize your org-agenda 
===================
_Display org properties in the agenda buffer_
![Screenshot](https://raw.github.com/Bruce-Connor/org-agenda-property/master/screenshot-2013-08-20--193404.jpg)

`org-agenda-property` is a package which **displays the properties** of an
org item beside (or below) the item's title **in the agenda buffer**. The
image above shows it being used to **display apointment locations**.


Customize the variable `org-agenda-property-list` to add which
properties you which to show.

Installation
===

If you have [Melpa](http://melpa.org/#/getting-started) configured, just issue 

    M-x package-install RET org-agenda-property
    
Variables
===

All variables can be edited by running
`org-agenda-property-customize` (_seriously, check it out, they
are just a few_ :-)). The documentations are mostly self
explanatory, I list here only the most important two.

1. `org-agenda-property-list`:  
   This should be a list of all properties you want displayed in the
   buffer. Default is `"LOCATION"`.
             
2. `org-agenda-property-position`:  
   This is where you want the properties to be displayed (besides the
   title or below the title?).
