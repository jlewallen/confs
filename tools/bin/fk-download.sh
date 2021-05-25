#!/bin/bash

set -e

IP=$1
PORT=$2
DRY=$3
if [ -z $IP ]; then
    echo "Missing station IP address."
    exit 2
fi

ROOT_URL=https://api.fkdev.org
ROOT_DIR=~/fieldkit/stations
INGESTION_URL=$ROOT_URL/ingestion
TIMESTAMP=`date +"%Y%m%d_%H%M%S"`

META="${TIMESTAMP}_meta.fkpb"
DATA="${TIMESTAMP}_data.fkpb"
STATUS="${TIMESTAMP}_status.json"

echo Querying $IP status...

fkc --address $IP --port $PORT --status --save $STATUS > /dev/null 2>&1

GENERATION=`jq -r .status.identity.generationId < $STATUS`
DEVICE_ID=`jq -r .status.identity.deviceId < $STATUS`

DEVICE_ID_HEX=`echo $DEVICE_ID | base64 -d | od -t x1 -An | tr -d '\040\011\012\015'`
GENERATION_HEX=`echo $GENERATION | base64 -d | od -t x1 -An | tr -d '\040\011\012\015'`

echo "device id: $DEVICE_ID_HEX"
echo "generation: $GENERATION_HEX"

DEVICE_DIR=$ROOT_DIR/$DEVICE_ID_HEX

mkdir -p $DEVICE_DIR

mv $STATUS $DEVICE_DIR

if [ -z $DRY ]; then
    echo downloading...

    curl -sv "http://$IP:$PORT/fk/v1/download/data?first=0&last=100" -o $DEVICE_DIR/$DATA 2> $DEVICE_DIR/$DATA.headers

    DATA_FK_BLOCKS=`grep Fk-Blocks $DEVICE_DIR/$DATA.headers | sed -e "s/< //"`

    echo "Data: $DATA_FK_BLOCKS"
else
    echo curl -sv "http://$IP:$PORT/fk/v1/download/data?first=0&last=100"
fi
