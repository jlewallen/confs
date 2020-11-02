export PATH=$HOME/tools/bin:$PATH

setxkbmap -option caps:super

xrandr --dpi

exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) i3
