#!/bin/bash
mkdir -p temp

while getopts d opt; do
    case "$opt" in
        d) debug="Y";;
    esac
done

if [ "$debug"="Y" ]; then
    sed -e "s/+DEBUG\*/       /g" gorbitsa.cbl > temp/gorbitsa.cbl
    cobc -x -O3 -W -o out/gorbitsa temp/gorbitsa.cbl 
else
    cobc -x -O3 -W -o out/gorbitsa gorbitsa.cbl 
fi
