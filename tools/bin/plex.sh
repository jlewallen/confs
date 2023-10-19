#!/bin/bash

docker run -d \
	--name=plex \
	--net=host \
	-e PUID=1000 \
	-e PGID=1000 \
	-e VERSION=docker \
	-e PLEX_CLAIM= `#optional` \
	-v /home/jlewallen/drive0/plex/config:/config \
	-v /home/jlewallen/drive0:/drive0 \
	-v /home/jlewallen/drive1:/drive1 \
	-v /home/jlewallen/drive2:/drive2 \
	-v /home/jlewallen/sync:/sync \
	-v /home/jlewallen/photos:/photos \
	--restart unless-stopped \
	lscr.io/linuxserver/plex:latest
