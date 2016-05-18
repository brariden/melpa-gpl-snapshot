Viking minor  mode enables you to  delete things at point  with one
key stroke at once. More and more will be deleted if you repeat the
key stroke.  As visual  feedback the  thing to  be deleted  will be
highlighted shortly.

The default key binding is C-d, but  you may also bind it to C-k or
whatever you wish.

If you press C-d the first time, the word at point will be deleted.
If you press it again, the remainder of the line from point will be
deleted. If pressed  again, the whole line, then  the paragraph and
finally the whole buffer will be deleted.

Like:
[keep pressing ctrl] C-d                  - del word
                     C-d C-d              - del line remainder
                     C-d C-d C-d          - del line
                     C-d C-d C-d C-d      - del paragraph
                     C-d C-d C-d C-d C-d  - del buffer

However, this only works when pressing the  key in a row. If you do
something  else in  between, it  starts from  scratch (i.e.  delete
word).
