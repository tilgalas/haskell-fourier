#!/bin/bash

set -e

echo "$(for f in `ls *.svg`
do
    echo $f -b white -e `basename -s .svg $f`.png
done)" | inkscape --shell

convert -delay 1x30 `ls *.png | sort -V` $1
	 
rm *.svg *.png
