#!/bin/sh

if [ -s $HOME/.xkb/keymap/mykeymap ]
then
    sleep 1
    xkbcomp -I$HOME/.xkb $HOME/.xkb/keymap/mykeymap $DISPLAY
fi
