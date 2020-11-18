#!/bin/bash

set -xe

for a in *.txt; do
	rm -f $a-backup
	cp $a $a-backup

	cat $a-backup | sed -r 's/\x1B\[([0-9]{1,3}(;[0-9]{1,2})?)?[mGK]//g' > $a
done

rm -f *-backup
