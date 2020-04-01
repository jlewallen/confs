exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) i3

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
