sudo docker run --detach \
	--hostname gitlab.example.com \
	--publish 7443:443 --publish 7080:80 --publish 7022:22 \
	--name gitlab \
	--restart always \
	--volume $GITLAB_HOME/config:/etc/gitlab:Z \
	--volume $GITLAB_HOME/logs:/var/log/gitlab:Z \
	--volume $GITLAB_HOME/data:/var/opt/gitlab:Z \
	--shm-size 256m \
	gitlab/gitlab-ee:latest
