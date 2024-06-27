#!/bin/bash
#
set -o noclobber

find . -mindepth 1 -type d -print0 | while IFS= read -r -d '' dir; do
	pushd "$dir"
	PREFIX=`openssl rand -hex 20 | head -c 6`

	find . -type f -print0 | while IFS= read -r -d '' file; do
		name=`basename "$file"`
		mv "$file" "${PREFIX}_${name}"
	done

	popd
done
