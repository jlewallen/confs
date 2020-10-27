#!/bin/bash

function setup() {
	# https://masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
	# Oddly, build-dep was no help on my laptop, for some reason so this has manual deps, too.
	sudo add-apt-repository ppa:ubuntu-toolchain-r/ppa \
        && sudo apt-get update -y \
        && sudo apt-get install -y gcc-10 libgccjit0 libgccjit-10-dev
	sudo apt-get install -y libjansson4 libjansson-dev git texinfo libgtk-3-dev libwebkit2gtk-4.0-dev libxpm-dev libjpeg-dev libtiff-dev gnutls-dev libgif-dev libmagickwand-dev libvterm-dev
	sudo sed -i 's/# deb-src/deb-src/' /etc/apt/sources.list
	sudo apt-get update && sudo apt-get build-dep -y emacs
}

set -xe

export CC="gcc-10"

pushd ~/tools/emacs/emacs

./autogen.sh

./configure \
	--prefix=/home/jlewallen/tools/emacs/installed \
	--enable-locallisppath=~/.emacs.d/lisp \
	--without-gconf \
	--without-toolkit-scroll-bars \
	--without-xaw3d \
	--without-gsettings \
	--with-gnutls \
	--with-rsvg \
	--with-modules \
	--with-x \
	--with-xml2 \
	--with-json \
	--with-imagemagick \
	--with-xwidgets \
	--with-mailutils \
	--with-nativecomp \
	CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"

make -j6

make install

popd
