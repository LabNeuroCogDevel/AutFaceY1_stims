#!/usr/bin/env bash

beares="/mnt/B/bea_res/"
[ ! -r "$beares" ] && echo "change beares location in $0" && exit 1
! which dos2unix >/dev/null && echo "need dos2unix!" && exit 1

[ ! -d Y1/epraw ] &&  mkdir -p Y1/epraw

cp $beares/Autism/Experiments\ \&\ Data/K\ Award/Scanner/Raw\ Data/Year\ 1/*/*{Cars,CMFT}-*txt Y1/epraw
cd Y1/epraw/
dos2unix *
