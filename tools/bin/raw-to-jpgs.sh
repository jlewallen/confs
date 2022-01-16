#!/bin/bash

for a in *.DNG; do
	n=`basename $a .DNG`
	dcraw -v -w $a
	convert $n.ppm $n.jpg
done

for a in *.CR2; do
	n=`basename $a .CR2`
	dcraw -v -w $a
	convert $n.ppm $n.jpg
done
