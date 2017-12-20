#!/usr/bin/env bash
set -u
set -e
set -x

JDEE_SRV_SRC="${HOME}/.emacs.d/jdee-server-src"
if [ -d ${JDEE_SRV_SRC} ];
then
    cd ${JDEE_SRV_SRC}
    git pull
else
    git clone https://github.com/jdee-emacs/jdee-server.git ${JDEE_SRV_SRC}
    cd ${JDEE_SRV_SRC}
fi

mvn clean package

JDEE_SRV_TARGET="${HOME}/.emacs.d/jdee-server"
if [ -d ${JDEE_SRV_TARGET} ];
then
    rm -rf ${JDEE_SRV_TARGET}/*.jar
else
    mkdir ${JDEE_SRV_TARGET}
fi
cp target/jdee-bundle*.jar ${JDEE_SRV_TARGET}
