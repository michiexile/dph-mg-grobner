#!/bin/bash

set -e

mirror=http://boxen.math.washington.edu/sage
platform=linux/64bit
version=sage-4.4.2-linux-64bit-ubuntu_10.04_lts-x86_64-Linux
format=tar.gz

packages() {
  echo '-------------------- Installing packages.' 1>&2
  aptitude install --assume-yes \
    curl \
    python python-dev python-setuptools \
    mysql-client-5.1 libmysqlclient-dev \
    mysql-server mercurial sysstat \
    build-essential
}

install_sage() {
  echo '-------------------- Installing SAGE.' 1>&2
  cd /opt
  if [ ! -x /opt/$version ]
  then
    curl "$mirror/$platform/$version.$format" | tar xz
  else
    [ -d /opt/$version ]
  fi
  if [ -x /opt/sage ]
  then
    [ -L /opt/sage ] && rm /opt/sage
  fi
  ln -s /opt/$version /opt/sage
  ln -s /opt/sage/sage /usr/local/bin/
  sed -i "s:....:/opt/sage:" /opt/sage/sage
  /opt/sage/sage -optional
  /opt/sage/sage -i openmpi
  /opt/sage/sage -i mpi4py
}

mysql_bindings() {
  echo '-------------------- Installing MySQL bindings for SAGE.' 1>&2
  cd /opt/$version
  ./sage -sh <<CMDS
  easy_install MySQL-python
CMDS
}

packages
install_sage
mysql_bindings

