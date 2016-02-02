#!/bin/bash

for f in `ls *.svg`
do
    inkscape -f $f -b white -e `basename -s .svg $f`.png
done

convert -delay 1x30 `ls *.png | sort -V` $1
	 
rm *.svg *.png
