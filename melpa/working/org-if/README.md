# org-if

**org-if** is a simple system for developing *Choose Your Own Adventure* style [Interactive Fiction](https://en.wikipedia.org/wiki/Interactive_fiction) (i.e. similar to [Twine](http://twinery.org/) using [GNU Emacs](https://www.gnu.org/software/emacs/) and [Org mode](http://orgmode.org/).

## Install 

### MELPA

The easiest way to install **org-if** is to get it from [MELPA](https://melpa.org/#/), 

#### Configuring Emacs to use MELPA

1. Open **Emacs**.

2. Press the **Alt**, **Shift**, and **;** keys at the same time. `Eval:` should appear in the lower-left corner of the window.

3. Type `(find-file user-init-file)` and press **Enter**.

4. Copy the following text and select *Edit->Paste* in the top menu of **Emacs** to paste the text into the file you just opened.

```elisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
             (package-initialize)
```

5. Select *File->Save* to save the file.

6. Quit and restart Emacs to make the changes take effect.

### Installing org-if with MELPA

1. Press the **Alt** and **x** keys at the same time. **M-x** should appear in the lower-left corner of the window.

2. Type `package-install` and press **Enter**. You should now see **Install package:**.

3. Type `org-if` and press **Enter**.

## Use

### Start 

These instructions will help you start your first **org-if** game.

1. Open **Emacs**.

2. Select *File->Open File...* in the top menu.

3. Navigate to the directory where you downloaded the **org-if** code. It is called *~/org-if* above.
   
4. Open the file *examples/simple-game/index.org*.

5. You should now see an item in the top menu with the name *Org-IF*. Click the checkbox for *Org-IF->Active*.
   
6. You have now started your first **org-if** game. 

### Play

Simply click on the underlined text under the second heading between pages.

### Save & Quit

If you want to stop the game while saving your place, select *Org-IF->Save & Quit* in the top menu.

### Restore

1. Open any *.org* file in the directory containing the game you want to resume.

2. Then select *Org-IF->Restore* in the top menu.
   
### Develop

I will write more extensive documentation soon. In the meantime, look at the files in [[file:examples/simple-game/][examples/simple-game]] for how to create a simple **org-if** game. 
 
## Appendix: Installing Emacs

If you do not already have a recent version of **Emacs** (i.e. 24.5 or later), install one for your system.
 
### Windows XP/Vista/7/8

**NOTE:** You will need to install [7-Zip](http://www.7-zip.org/a/7z920.exe) to unpack *emacs-bin*.
     
1. Download [emacs-bin](http://sourceforge.net/projects/emacs-bin/files/latest/download).
   
2. Open the file in **7-Zip** and extract the file contents to a directory of your choice.

3. Open your file manager and navigate to the directory you chose in the previous step.

4. Open the *bin* subdirectory and run **addpm.exe** to add **Emacs** to the Start Menu.

5. You have now installed **Emacs** under Windows. Congratulations!
   
### MacOS X

1. Download an [OS X version](http://emacsformacosx.com/) of **Emacs**.

2. Click on the file you just downloaded.
   
3. Drag the file *Emacs* to your *Applications* folder.

4. You have now installed **Emacs** for OSX!
   
### Ubuntu/Mint/Debian
     
1. Open the **Terminal** application and run the following command.

```sh
sudo apt-get install emacs
```

2. You have installed **Emacs** on Linux with a single command! Aren't you swell?

### Other Systems

If you know enough to install and use them, you know enough to install **Emacs**.

