#!/bin/sh
exec /usr/bin/emacsclient -n -e \
'(progn
  (dolist (f (cdr-safe (reverse (frame-list))))
    (delete-frame f t))
  (kill-emacs))'
