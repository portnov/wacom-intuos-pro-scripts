#!/bin/bash

if [ $# != 1 ]
then echo "Usage: wacom-ring-mode.sh {0|1|2|3}"
     exit 0
fi

MODE=$1

PAD=$(xsetwacom list dev | grep -o ".*pad")

case $MODE in
  0) RINGDOWN="key ["
     RINGUP="key ]" ;;
  1) RINGDOWN="button 5"
     RINGUP="button 4" ;;
  2) RINGDOWN="key PgDn"
     RINGUP="key PgUp" ;;
  3) RINGDOWN="key ctrl ]"
     RINGUP="key ctrl [" ;;
  *) echo "Unknown mode $MODE"
     exit 1
     ;;
esac

xsetwacom set "$PAD" AbsWheelDown "$RINGDOWN"
xsetwacom set "$PAD" AbsWheelUp "$RINGUP"


