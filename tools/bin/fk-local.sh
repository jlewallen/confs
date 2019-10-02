#!/bin/bash

URL=http://127.0.0.1:8080

if [ -z $FK_AUTH ]; then
    export FK_AUTH=`curl -si $URL/login --data "{\"email\":\"$FK_EMAIL\",\"password\":\"$FK_PASSWORD\"}" | grep -i Authorization | sed -e "s/Authorization: Bearer //i" | tr -d '\n' | tr -d '\r'`
fi

curl -s -H "Authorization: Bearer $FK_AUTH" "$@"
