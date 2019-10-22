#!/bin/bash

set -ex

IP=$1
if [ -z $IP ]; then
    echo "Missing station IP address."
    exit 2
fi

PORT=80
ROOT_URL=https://api.fkdev.org
ROOT_DIR=~/fieldkit/stations
INGESTION_URL=$ROOT_URL/ingestion
TIMESTAMP=`date +"%Y%m%d_%H%M%S"`

META="${TIMESTAMP}_meta.fkpb"
DATA="${TIMESTAMP}_data.fkpb"
STATUS="${TIMESTAMP}_status.json"
PORTAL_SUMMARY="${TIMESTAMP}_portal.json"

echo Querying status...

fkdevice-cli --address $IP --port $PORT --status --save $STATUS > /dev/null 2>&1

GENERATION=`jq -r .status.identity.generation < $STATUS`
DEVICE_ID=`jq -r .status.identity.deviceId < $STATUS`

echo Querying block sync...

DEVICE_ID_HEX=`echo $DEVICE_ID | base64 -d | od -t x1 -An | sed "s/ //g"`
DEVICE_DIR=$ROOT_DIR/$DEVICE_ID_HEX

mkdir -p $DEVICE_DIR

mv $STATUS $DEVICE_DIR

fk-dev.sh $ROOT_URL/data/devices/$DEVICE_ID_HEX/summary > $DEVICE_DIR/$PORTAL_SUMMARY

LAST_GENERATION=`jq -r ".provisions[0].generation" < $DEVICE_DIR/$PORTAL_SUMMARY`
LAST_META=`jq -r ".provisions[0].meta.last" < $DEVICE_DIR/$PORTAL_SUMMARY`
LAST_DATA=`jq -r ".provisions[0].data.last" < $DEVICE_DIR/$PORTAL_SUMMARY`

if [ $GENERATION == $LAST_GENERATION ]; then
    echo "Same Gen"
else
    LAST_META=0
    LAST_DATA=0
fi

echo $GENERATION
echo $LAST_GENERATION
echo $LAST_META
echo $LAST_DATA

echo Downloading...

curl -sv "http://$IP:$PORT/fk/v1/download/meta?first=$LAST_META" -o $DEVICE_DIR/$META 2> $DEVICE_DIR/$META.headers
curl -sv "http://$IP:$PORT/fk/v1/download/data?first=$LAST_DATA" -o $DEVICE_DIR/$DATA 2> $DEVICE_DIR/$DATA.headers

META_FK_BLOCKS=`grep Fk-Blocks $DEVICE_DIR/$META.headers | sed -e "s/< //"`
DATA_FK_BLOCKS=`grep Fk-Blocks $DEVICE_DIR/$DATA.headers | sed -e "s/< //"`

echo Uploading...

fk-dev.sh -H "$META_FK_BLOCKS" -H "Fk-Type: meta" -H "Fk-DeviceId: $DEVICE_ID" -H "Fk-Generation: $GENERATION" --data-binary @$DEVICE_DIR/$META $INGESTION_URL
fk-dev.sh -H "$DATA_FK_BLOCKS" -H "Fk-Type: data" -H "Fk-DeviceId: $DEVICE_ID" -H "Fk-Generation: $GENERATION" --data-binary @$DEVICE_DIR/$DATA $INGESTION_URL
