#!/bin/bash

docker run -d --name mqttd -p 1883:1883 -p 1884:1884 -p 3030:3030 -it bytebeamio/rumqttd
