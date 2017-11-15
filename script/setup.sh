#!/bin/bash
#=#=#=
# This script synclonize my local enviroment.
#=#=

pacman -S xmonad xmonad-contrib xmobar xorg-server xorg-xinit git nvim emacs adobe-source-hans-jp-fonts otf-ipafont fcitx-mozc fcitx-gtk3 fcitx-configtool texlive-core texlive-langjapanese ghostscript poppler-data zathura feh zathura-pdf-poppler zsh
git clone https://ShogoNakano@bitbucket.org/ShogoNakano/home.git
chsh -s /usr/bin/zsh yoshizu
