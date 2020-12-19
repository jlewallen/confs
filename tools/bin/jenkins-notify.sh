#!/bin/bash

source ~/.zshenv.private.sh

INTERVAL=10
NOTIFY=/usr/bin/notify-send
URL=https://${JENKINS_AUTH}@code.conservify.org/jenkins/rssAll

trap "kill -TERM -$$" SIGINT SIGTERM

rsstail -P -u "${URL}" -i 30 -n 0 | \
	while read line; do
		$NOTIFY "${line:7}"
	done
