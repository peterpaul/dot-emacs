#!/bin/sh

EXWM_DIR=$HOME/.emacs.d/stow/exwm
sudo cp $EXWM_DIR/usr/local/bin/exwm-start /usr/local/bin/exwm-start
sudo cp $EXWM_DIR/usr/share/xsessions/exwm.desktop /usr/share/xsessions/exwm.desktop
