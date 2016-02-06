#!/bin/bash

set -e

echo -e "background.svg -b white -e background.png\n
$(for f in `ls graph*.svg`
do
    echo $f -e `basename -s .svg $f`.png
done)" | inkscape --shell

convert  -delay 0 background.png -dispose previous -delay 1x30 `ls graph*.png | sort -V` -coalesce -delete 0 -loop 0 -layers optimize $1
	 
rm *.svg *.png
