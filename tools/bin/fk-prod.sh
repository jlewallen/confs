#!/bin/bash

BASE_URL=https://api.fieldkit.org

if [ -z $FK_AUTH ]; then
    export FK_AUTH=`curl -si $BASE_URL/login --data "{\"email\":\"$FK_EMAIL\",\"password\":\"$FK_PASSWORD\"}" | grep -i Authorization | sed -e "s/Authorization: Bearer //i" | tr -d '\n' | tr -d '\r'`
fi

if [ $# -eq 1 ]; then
	if [[ $1 == http* ]]; then
		curl -s -H "Authorization: Bearer $FK_AUTH" "$@"
	else
		curl -s -H "Authorization: Bearer $FK_AUTH" "$BASE_URL$1"
	fi
else
	curl -s -H "Authorization: Bearer $FK_AUTH" "$@"
fi
