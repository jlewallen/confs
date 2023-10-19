#!/bin/bash

library_path=~/shares/photos/library
extension=ARW
dry=N

options=$(getopt -o -n --long extension:,library: -- "$@")
[ $? -eq 0 ] || {
    echo "Invalid options"
    exit 1
}
eval set -- "$options"
while true; do
    case "$1" in
	-n)
		dry=Y
		;;
    --library)
        shift; # The arg is next in position args
        library_path=$1
        ;;
    --extension)
        shift; # The arg is next in position args
        extension=$1
        ;;
    --)
        shift
        break
        ;;
    esac
    shift
done

function move_files {
	from_dir=$1
	fr=$2
	to=$3
	skip=false

	for a in ${to}/${fr}*; do
		if [ -f ${a} ]; then
			skip=true
		fi
	done

	if [ "$skip" = true ]; then
	   return 2
	fi

	if [ ! -d ${to} ]; then
		echo mkdir -p ${to}
	fi
	echo rsync -ua ${from_dir}/${fr}* ${to}

	if [[ $dry = "N" ]]; then
		if [ ! -d ${to} ]; then
			mkdir -p ${to}
		fi
		rsync -ua ${from_dir}/${fr}* ${to}
	fi

	return 0
}

for raw_fn in `find . -type f -iname "*.${extension}" | sort`; do
	from_dir=`dirname $raw_fn`
	name=`basename $raw_fn`
	rfn=`basename $raw_fn .${extension}`
	dir=`exiftool -s -s -s '-CreateDate' -d '%Y%m/%d' ${raw_fn}`
	# date=`exiftool -s -s -s '-CreateDate' -d '%Y%m%d_%H%M%S%z' ${raw_fn}`
	new_dir=${library_path}/${dir}
	if move_files ${from_dir} ${rfn} ${new_dir}; then
		echo copy ${name} ${dir} rsync ${rfn}* ${new_dir}
	else
		echo skip ${name} ${dir} rsync ${rfn}* ${new_dir}
	fi
done
