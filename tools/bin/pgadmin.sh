#!/bin/bash

sudo mkdir -p ~/drive2/pgadmin
sudo chown -R 5050:5050 ~/drive2/pgadmin

docker pull dpage/pgadmin4

docker run -p 8070:80 --name pgadmin -d \
	-v ~/drive2/pgadmin:/var/lib/pgadmin \
	-e 'PGADMIN_DEFAULT_EMAIL=jacob@conservify.org' \
	-e 'PGADMIN_DEFAULT_PASSWORD=asdfasdfasdf' \
	-e 'PGADMIN_CONFIG_CONSOLE_LOG_LEVEL=10' \
	-e 'PGADMIN_BIND_ADDRESS=0.0.0.0:80' \
	-e 'PGADMIN_LISTEN_ADDRESS=0.0.0.0:80' \
	dpage/pgadmin4
