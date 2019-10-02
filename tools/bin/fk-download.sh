#!/bin/bash

set -ex

IP=192.168.0.165
PORT=80
URL=http://192.168.0.100:8080/ingestion
ROOT_DIR=/tmp/archive
AUTHORIZATION="Authorization: Bearer eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImphY29iQGNvbnNlcnZpZnkub3JnIiwiZXhwIjoxNTY4OTA5MzY2LCJpYXQiOjE1Njg4MjI5NjYsInJlZnJlc2hfdG9rZW4iOiJucGcxWm95bDZ5Y3JCeS1JUWM4UW1UNm5PMVkiLCJzY29wZXMiOiJhcGk6YWNjZXNzIiwic3ViIjoyfQ.dG5A6tvHxXCSVhatKS06BkRobHm-2xXbT-jC5wAqu0SNUOW7ueFwhUk_7FBEx9HCBRPinvqav1emv0P79DIsxA"

META="meta.fkpb"
DATA="data.fkpb"
STATUS="status.json"

echo Querying status...

DIR=$ROOT_DIR
mkdir -p $DIR

fkdevice-cli --address $IP --port $PORT --status --save $STATUS > /dev/null 2>&1

GENERATION=`jq -r .status.identity.generation < $STATUS`
DEVICE_ID=`jq -r .status.identity.deviceId < $STATUS`

echo Downloading...

curl -sv "http://$IP:$PORT/fk/v1/download/meta?first=0" -o $META 2> $META.headers
curl -sv "http://$IP:$PORT/fk/v1/download/data?first=0" -o $DATA 2> $DATA.headers

META_FK_BLOCKS=`grep Fk-Blocks $META.headers | sed -e "s/< //"`
DATA_FK_BLOCKS=`grep Fk-Blocks $DATA.headers | sed -e "s/< //"`

echo Uploading...

curl -H "$AUTHORIZATION" -H "$META_FK_BLOCKS" -H "Fk-Type: meta" -H "Fk-DeviceId: $DEVICE_ID" -H "Fk-Generation: $GENERATION" --data-binary @$META $URL
curl -H "$AUTHORIZATION" -H "$DATA_FK_BLOCKS" -H "Fk-Type: data" -H "Fk-DeviceId: $DEVICE_ID" -H "Fk-Generation: $GENERATION" --data-binary @$DATA $URL
