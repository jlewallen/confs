#!/bin/bash

NAME=$1
if [ -z "$NAME" ]; then
	echo "Usage:"
	echo "copy-card.sh <NAME>"
	exit 2
fi

set -xe

mkdir -p /tmp/card
mkdir -p  ~/cards/$1
mount /dev/sde1 /tmp/card
rsync -vua /tmp/card/ ~/cards/$1/
chown -R jlewallen. ~/cards/$1
rm -rf '/tmp/card/*.chk'
rm -rf '/tmp/card/*.bin'
rm -rf '/tmp/card/2020*'
cp ~/fieldkit/firmware/build/samd51/fk/fk-bundled-fkb.bin /tmp/card
cp ~/fieldkit/firmware/build/samd51/bootloader/fkbl-fkb.bin /tmp/card
umount /tmp/card
