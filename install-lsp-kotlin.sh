#!/bin/sh
rm -rf ~/.emacs.d/kotlin-language-server
cd ~/.emacs.d
git clone https://github.com/fwcd/kotlin-language-server.git
cd kotlin-language-server
./gradlew :server:distTar
cd ~/local
tar xvf ~/.emacs.d/kotlin-language-server/server/build/distributions/server-0.3.0.tar
cd ~/bin
ln -s ~/local/server-0.3.0/bin/kotlin-language-server

