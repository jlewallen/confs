dir1=$1
dir2=$2

pushd $dir1
find . -type f -printf "%p %s\n" | sort > /tmp/dir1.txt
popd
pushd $dir2
find . -type f -printf "%p %s\n" | sort > /tmp/dir2.txt
popd
diff /tmp/dir1.txt /tmp/dir2.txt
