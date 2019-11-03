#!/bin/bash

DEVICE=$1

function usage() {
    exit 2
}

if [ -z $DEVICE ]; then
    usage
fi

sudo whoami

pushd ~/fieldkit/firmware
make fw -j4
popd

mkdir -p /tmp/card
sudo mount $DEVICE /tmp/card
sudo cp ~/fieldkit/firmware/build/samd51/bootloader/*.bin /tmp/card
sudo cp ~/fieldkit/firmware/build/samd51/fk/*.bin /tmp/card
ls -alh /tmp/card
sudo umount /tmp/card
