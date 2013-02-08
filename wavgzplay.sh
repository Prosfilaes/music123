#!/bin/sh
FILENAME=`tempfile -s .wav`
zcat "$1" > $FILENAME
music123 $FILENAME
rm $FILENAME
