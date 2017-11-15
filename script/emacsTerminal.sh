#!/bin/sh
XMODIFIERS="@im=none"
GTK_IM_MODULE=none
TERM=screen-256color-bce
export XMODIFIERS GTK_IM_MODULE TERM
exec /usr/bin/emacsclient  --tty "$@"
