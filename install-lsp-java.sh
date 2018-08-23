#!/bin/sh
rm -rf ~/.emacs.d/eclipse.jdt.ls/server/
mkdir -p ~/.emacs.d/eclipse.jdt.ls/server/
wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz -O /tmp/jdt-latest.tar
tar xf /tmp/jdt-latest.tar -C ~/.emacs.d/eclipse.jdt.ls/server/

