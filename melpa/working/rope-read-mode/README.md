<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orga40ab7c">1. Commentary</a>
<ul>
<li><a href="#org2a30a15">1.1. About rope-read</a></li>
<li><a href="#orgb38b841">1.2. Usage</a></li>
<li><a href="#org9d17f37">1.3. Install</a></li>
<li><a href="#org5cc99e1">1.4. Dependencies</a></li>
<li><a href="#org2a13f93">1.5. Development</a></li>
<li><a href="#orgb397850">1.6. Related</a></li>
<li><a href="#orgecf4122">1.7. History</a></li>
</ul>
</li>
<li><a href="#orge3f1f34">2. Code</a>
<ul>
<li><a href="#orgbdf3952">2.1. Variables</a></li>
<li><a href="#orgf6bcaea">2.2. Keys</a></li>
<li><a href="#orgacb548a">2.3. Mode rope-read</a></li>
<li><a href="#org9974ad2">2.4. Management and Navigation</a></li>
<li><a href="#org83b3aac">2.5. Y-coordinates of a line</a></li>
<li><a href="#orgd6650bb">2.6. Reverse every other line</a></li>
<li><a href="#org85c91b8">2.7. Snap the line which contains point</a></li>
<li><a href="#org3520778">2.8. Provide the file as library</a></li>
</ul>
</li>
</ul>
</div>
</div>


# Commentary<a id="orga40ab7c"></a>

## About rope-read<a id="org2a30a15"></a>

### What<a id="org2c874f8"></a>

`rope-read-mode` can reverse every other line of a buffer or in a part
of a buffer.

With every other line reversed reading can be like following a rope.

### Illustration<a id="org9c7b50e"></a>

[![img](./rope-read-illustration.png)](rope-read-illustration.png)

### Ad<a id="org216adfa"></a>

-   Are you tired moving the look from one end of a line to the other
    end just because of a line break?

-   Are you too lazy moving the look from one end of a line to the
    other end just because of a line break?

-   Do you think about the line break coercing the brutal change of the
    look from one side to the other as humiliating?

-   Do you want to read three or four lines more of a text before you
    fall asleep?

More than zero yes answers?

Then rope read might help!

### Why<a id="org98dfb6c"></a>

-   Chill.  `rope-read-mode` allows fluent reading.
    -   Find the start of the next line easily.

    -   Avoid stressful eye movement.

-   Have an alternative view on text.

### Learn<a id="org53b6b2b"></a>

Typically you need to practice for some time to be able to read
upside-down lines effortlessly.

## Usage<a id="orgb38b841"></a>

### Turning it on and off<a id="org2ed7e9c"></a>

`M-x rope-read-mode` in a buffer activates rope-read.  No visible
change in the buffer is to be expected.  The buffer is set read-only.

Type `M-x rope-read-mode` or press 'q' to quit rope-read.  The buffer
writability gets restored.

### Action<a id="orgbda4737"></a>

When `rope-read-mode` is on you can press

-   `C-g` to interrupt `rope-read-mode` anytime,
-   `g` to trigger reversing every other line for the currently visible
    part of the buffer,
-   `r` to go back to the representation of the buffer without reversed
    lines (keeping `rope-read-mode`),
-   `d` to reverse every other line starting with the line below the
    cursor,
-   `p` to reverse every other line starting with the line below the
    cursor up to the end of the paragraph (if visible) and move point
    there.
-   The next four commands are followed by reversing every other line:
    -   `SPC` to scroll a screen down,
    -   `<backspace>` or `S-SPC` to scroll a screen up,
    -   `v` or `<return>` to scroll one line down,
    -   `V` or `y` to scroll one line up,
-   `?` to open the help buffer,
-   `q` to quit `rope-read-mode`.

For convenience you can bind command `rope-read-mode` to a key.  For
example to activate or deactivate `rope-read-mode` by pressing scroll
lock two times use the line

    (global-set-key (kbd "<Scroll_Lock> <Scroll_Lock>") 'rope-read-mode)

### Image files<a id="org2512853"></a>

The reverse representation of lines is realized with images.  They get
collected in directory `rope-read-image-overlay-path` which defaults
to `~/.emacs.d/rope-reading`.  You can delete this directory any time.

### Security<a id="org778192b"></a>

`rope-read-mode` does not change the content of a buffer.  Data loss
has not been reported yet.

Since the overlay-image files get stored on disk this could be a
security issue.  E.g. when you use `rope-read-mode` to look at your
super secret password file.

### Beep<a id="org67b3d22"></a>

The system beep can be annoying.  The line

    amixer set Beep off

silences the beep on some systems.  Precondition is that you have the
`amixer` program ready.

## Install<a id="org9d17f37"></a>

### Emacs Package<a id="org88facd8"></a>

`rope-read-mode` is available as MELPA package
[![img](http://melpa.org/packages/rope-read-mode-badge.svg)](http://melpa.org/#/rope-read-mode) and ready
immediately after the install.

### Install from el file<a id="org8610b41"></a>

If you just have the emacs-lisp file then:

-   load the file into Emacs
-   do `M-x eval-buffer`

That's it.  You installed rope-read-mode and `M-x rope-read-mode` is
available.

## Dependencies<a id="org5cc99e1"></a>

-   Emacs is running under X.
-   The programm `convert` of the ImageMagick-suite is available.

The `convert` program has the job to create images of lines and rotate
them.

## Development<a id="org2a13f93"></a>

### Known Bugs<a id="org248b5ba"></a>

-   rope-read-mode sometimes spontaneously fails.
    -   In this case a refresh with `g` might help.
    -   You can always try `C-g q` and start again.
-   rope-read-mode often does not work for org-mode files.
    -   Possibly this is due to the interference of overlays of org and
        rope-read.

### Wishes<a id="org49a5cf2"></a>

-   Quicker transformation.

### Vision<a id="orgffc1fa8"></a>

`rope-read-mode` gets `rope-mode` which allows also editing.
`rope-mode` would provide a further editing feeling maybe.

### Lentic Literate Style<a id="org6f433dc"></a>

This program is written in emacs lisp in lentic style based on the
'lentic' package [![img](http://melpa.org/packages/lentic-badge.svg)](http://melpa.org/#/lentic).

This means the that this file can be regarded just as an emacs lisp
file.  But actually this file contains extra comments which allow the
interpretation of the file as Org file.  Lentic-mode makes it easy to
write this style.

A possible initialization of lentic is this:

    (global-lentic-start-mode)

Find more about lentic at
[![img](http://melpa.org/packages/lentic-badge.svg)](http://melpa.org/#/lentic).

### Communication<a id="orgf59d824"></a>

Use the GitHub infrastructure i.e. pull requests or
<https://github.com/marcowahl/rope-read-mode/issues>.  Or contact the
author directly.

### Contribution<a id="orgb44bed8"></a>

Contributions in any respect are welcome, e.g. ideas and improvements.

### Contributors<a id="org3054e9e"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Syohei YOSHIDA</td>
</tr>


<tr>
<td class="org-left">Marco WAHL</td>
</tr>
</tbody>
</table>

## Related<a id="orgb397850"></a>

-   rope-read for firefox at
    <https://greasyfork.org/en/scripts/10634-rope-read>
-   'spray' which is available as Elpa package
    [![img](http://melpa.org/packages/spray-badge.svg)](http://melpa.org/#/spray) realizes another
    alternative view mode.
-   'fliptext' which also is available as Elpa package
    [![img](http://melpa.org/packages/fliptext-badge.svg)](http://melpa.org/#/fliptext) realizes an

    ˙ʇxǝʇ pǝddılɟ ɹoɟ poɥʇǝɯ-ʇnduı

## History<a id="orgecf4122"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-right">201501151211</td>
<td class="org-left">v0.1 New option rope-read-calculate-exact-y-coordinates</td>
</tr>


<tr>
<td class="org-right">201501311657</td>
<td class="org-left">v0.2 Replace whenever a line is ready</td>
</tr>


<tr>
<td class="org-right">201503160841</td>
<td class="org-left">Dropped option heuristic y-coordinates calculation</td>
</tr>


<tr>
<td class="org-right">201503161010</td>
<td class="org-left">v0.3 Operations based on visual movement-commands</td>
</tr>


<tr>
<td class="org-right">201508081255</td>
<td class="org-left">v0.3.1 rope-read-mode starts line reversing at point</td>
</tr>


<tr>
<td class="org-right">201510202326</td>
<td class="org-left">v0.3.2 rope-read-mode does nothing at start</td>
</tr>


<tr>
<td class="org-right">201511182342</td>
<td class="org-left">Paragraph wise rope-read is useful.</td>
</tr>


<tr>
<td class="org-right">201602082358</td>
<td class="org-left">One scan through the documentation</td>
</tr>
</tbody>
</table>

# Code<a id="orge3f1f34"></a>

## Variables<a id="orgbdf3952"></a>

    (defvar rope-read-overlays nil
      "List of rope-read-overlays.")

    (defvar rope-read-olimid-next-unused 0
      "Overlay-image-id that has not been used yet.

      The program must reset this variable reasonably when an id gets
      used.")

    (defvar rope-read-image-overlay-path "~/.emacs.d/rope-reading/"
      "Path where the overlay images get stored.")

    (defvar rope-read-image-overlay-filename-format-string
      (concat (file-name-directory rope-read-image-overlay-path) "%d.png")
      "Template for the filenames to be written to disk.")

    (defvar rope-read-mode nil)
    (make-variable-buffer-local 'rope-read-mode)

    (defvar rope-read-old-buffer-read-only)
    (make-variable-buffer-local 'rope-read-old-buffer-read-only)

## Keys<a id="orgf6bcaea"></a>

    (defvar rope-read-mode-hook nil)

    (defvar rope-read-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map " " #'rope-read-next-page)
        (define-key map [?\S-\ ] #'rope-read-prev-page)
        (define-key map (kbd "<backspace>") #'rope-read-prev-page)
        (define-key map (kbd "<return>") #'rope-read-scroll-up-line)
        (define-key map "v" #'rope-read-scroll-up-line)
        (define-key map "y" #'rope-read-scroll-down-line)
        (define-key map "V" #'rope-read-scroll-down-line)
        (define-key map "g" #'rope-read-refresh)
        (define-key map "d" #'rope-read-reol)
        (define-key map "p" #'rope-read-next-paragraph)
        (define-key map "r" #'rope-read-delete-overlays)
        (define-key map "q" #'rope-read-quit)
        (define-key map "?" #'describe-mode)
        map)
      "Keymap for ‘rope-read-mode’.")

    (defvar rope-read-transform-fun
      ;; #'rope-read-reol-in-visible-buffer-part-with-images
      #'rope-read-reol
      "The function which transforms a screen for rope-reading.

    This indirection is for the comfort of any coder to try
    out something new.")

## Mode rope-read<a id="orgacb548a"></a>

    ;;;###autoload
    (define-minor-mode rope-read-mode
      "Rope Reading mode.

    In rope-read-mode every other line gets reversed.  rope-read-mode is a
    view only mode.

    \\{rope-read-mode-map}

    This mode can help to save eye movements.

    By reversing every other line the reader often just can dip the
    gaze at the end of a line to read on instead of doing the
    annoying search for the next line at the other side of the text."
      :lighter " rope-read" :keymap rope-read-mode-map
      (if rope-read-mode (rope-read-mode-enable) (rope-read-mode-disable)))

    (defun rope-read-mode-enable ()
      (unless (file-exists-p rope-read-image-overlay-path)
        (make-directory rope-read-image-overlay-path))
      (setq rope-read-old-buffer-read-only buffer-read-only
            buffer-read-only t)
      (run-hooks 'rope-read-mode-hook))

    (defun rope-read-mode-disable ()
      (rope-read-delete-overlays)
      (setq buffer-read-only rope-read-old-buffer-read-only))

## Management and Navigation<a id="org9974ad2"></a>

    (defun rope-read-delete-overlays ()
      "Delete all overlays currently used with the rope-read-feature."
      (interactive)
      (mapc #'delete-overlay rope-read-overlays)
      (setq rope-read-overlays nil))

    (defun rope-read-next-page ()
      (interactive)
      (rope-read-delete-overlays)
      (scroll-up-command)
      (redisplay t)
      (move-to-window-line 0)
      (funcall rope-read-transform-fun))

    (defun rope-read-prev-page ()
      (interactive)
      (rope-read-delete-overlays)
      (scroll-down-command)
      (redisplay t)
      (move-to-window-line 0)
      (funcall rope-read-transform-fun))

    (defun rope-read-scroll-line (n)
      "Scroll the buffer N lines and reverse every other visible line."
      (rope-read-delete-overlays)
      (scroll-up-line n)
      (redisplay t)
      (move-to-window-line 0)
      (funcall rope-read-transform-fun))

    (defun rope-read-scroll-up-line (n)
      "Scroll the buffer up N lines and reverse every other visible line.

      E.g.  for N = 1 the second-line becomes first."
      (interactive "p")
      (unless n (setq n 1))
      (rope-read-scroll-line n))

    (defun rope-read-scroll-down-line (n)
      "Scroll the buffer down N lines and reverse every other line.

      E.g.  for N = 1 the first-line becomes second."
      (interactive "p")
      (unless n (setq n 1))
      (rope-read-scroll-line (- n)))

    (defun rope-read-refresh ()
      "Refresh the rope-read-representation for the given window."
      (interactive)
      (rope-read-delete-overlays)
      (redisplay t)
      (move-to-window-line 0)
      (funcall rope-read-transform-fun))

    (defun rope-read-quit ()
      (interactive)
      (when rope-read-mode (rope-read-mode 'toggle)))

## Y-coordinates of a line<a id="org83b3aac"></a>

### Exact y-coordinate calculation of a line<a id="orgdc115a6"></a>

This function calculates the y-coordinates straightforward.  This
function takes a lot of time.

    (defun rope-read-y-info-of-line ()
      "Return the top coordinate and the height of the line that contains `(point)'.
    This function typically takes a while."
      (let* ((beg (progn (beginning-of-visual-line) (point)))
             (posn-at-point
              (progn
                (posn-at-point (point))))
             (y-top (cdr (posn-x-y posn-at-point)))
             (height (cdr (nth 9 posn-at-point)))
             (end (progn (end-of-visual-line) (point))))
        (goto-char beg)
        (while (and (< (point) (point-max))
                    (progn (forward-char)
                           (< (point) end)))
          (setq
           posn-at-point (posn-at-point (point))
           height (max height (cdr (nth 9 posn-at-point)))
           y-top (min y-top (cdr (posn-x-y posn-at-point)))))
        (cons y-top height)))

### TODO Try to speed up the function<a id="org893115e"></a>

Try to answer first: Is the speed up possible?

## Reverse every other line<a id="orgd6650bb"></a>

    (defun rope-read-reol-in-visible-buffer-part-with-images ()
      "Reverse every other line in the visible buffer part."
      (move-to-window-line 0)
      (rope-read-reol))

    (defun rope-read-advance-one-visual-line ()
      (beginning-of-visual-line 2))

    (defun rope-read-reol ()
      "Reverse every other line in the visible part starting with line after point."
      (interactive)
      (save-excursion
        (let ((point-at-start (point))
              (last-line
               (progn (move-to-window-line -1)
                      (point))))
          (goto-char point-at-start)
          (beginning-of-visual-line)
          (rope-read-advance-one-visual-line)
          (while (and (< (point) last-line) ; todo: handle case of last line
                      (< (save-excursion (end-of-visual-line) (point))
                         (point-max))) ; todo: try to handle also the very
                                            ; last line.  the last line is
                                            ; special because it is
                                            ; special for the
                                            ; beginning-of-visual-line
                                            ; command.  no further
                                            ; iteration!
            (rope-read-snap-visual-line-under-olimid-filename)
            (let* ((l-above (save-excursion (beginning-of-visual-line 0) (point)))
                   (l-beg   (save-excursion (beginning-of-visual-line) (point)))
                   (l-end   (save-excursion (end-of-visual-line) (point)))
                   (l-next  (save-excursion
                              (goto-char l-beg) (beginning-of-visual-line 2) (point)))
                                            ; try to use for identify truncation of the line
                   (olimid-current (1- rope-read-olimid-next-unused)))
              (push (make-overlay l-beg l-end) rope-read-overlays)
              (overlay-put
               (car rope-read-overlays) 'display
               (create-image
                (expand-file-name
                 (format
                  rope-read-image-overlay-filename-format-string
                  olimid-current))
                nil nil
                :ascent 'center
                ;; TODO: try to refine.  hint: try
                ;; understand.  is this a font-dependent
                ;; thing?  e.g. :ascent 83 is possible.
                ;; there are further attributes...
                ))
              (when (= l-end l-next)
                (overlay-put (car rope-read-overlays) 'after-string "\n")
                ;; this newline makes the images appear in some cases.
                ;; todo: at least think about doing something similar in
                ;; the analog case of 'before'.
                )
              (goto-char l-next)
              (redisplay t)
              (rope-read-advance-one-visual-line))))))

## Snap the line which contains point<a id="org85c91b8"></a>

For testing: (local-set-key (kbd "<f8>")
'rope-read-snap-visual-line-under-olimid-filename)

    (defun rope-read-snap-visual-line-under-olimid-filename ()
      "Snapshot the visual line with `(point)' flipflopped.

    Also consider the line above the line containing `(point)'.  If
    the line above is longer then extend the snapshot to use the
    length of the line above.  This often eases continuation of
    reading for short lines.

    The file name for the snapshot contains the number
    `rope-read-olimid-next-unused' as index.  Use the source for all
    detail."
      (interactive "P")
      (save-excursion
        (let* ((beg (progn (beginning-of-visual-line) (point)))
               (end (progn (end-of-visual-line) (point)))
               (end-above (save-excursion (goto-char beg) (end-of-visual-line 0) (point)))
               (beg-next (progn  (goto-char beg) (beginning-of-visual-line 2) ))
               (width (if (or (= end beg-next) (= end-above beg))
                          (- (nth 2 (window-inside-pixel-edges))
                             (nth 0 (window-inside-pixel-edges)))
                        (- (max (car (posn-x-y (posn-at-point end)))
                                (car (posn-x-y (posn-at-point end-above))))
                           (car (posn-x-y (posn-at-point beg))))))
               (y-info-getter #'rope-read-y-info-of-line)
               (y-top-height (progn (goto-char beg)
                                    (funcall y-info-getter)))
               (y-pos-line (car y-top-height))
               (height (cdr y-top-height))
               (x-win-left (nth 0 (window-inside-pixel-edges)))
               (y-win-top (nth 1 (window-inside-pixel-edges)))
               (x-anchor (+ x-win-left))
               (y-anchor (+ y-win-top y-pos-line)))
          (call-process
           "convert" nil nil nil
           (format "x:%s[%dx%d+%d+%d]"
                   (frame-parameter nil 'window-id)
                   width height x-anchor y-anchor)
           "-flip"
           "-flop"
           (expand-file-name
            (format
             rope-read-image-overlay-filename-format-string
             ((lambda ()
                (1-
                 (setq
                  rope-read-olimid-next-unused
                  (1+ rope-read-olimid-next-unused)))))))))))

## Provide the file as library<a id="org3520778"></a>

    (provide 'rope-read-mode)
