#!/bin/bash

if [ $# != 1 ]
then echo "Usage: wacom-ring-mode.sh {0|1|2|3}"
     exit 0
fi

MODE=$1

PAD=$(xsetwacom list dev | grep -o ".*pad")
STYLUS=$(xsetwacom list dev | grep -o ".*stylus")
TOUCH=$(xsetwacom list dev | grep -o ".*touch")

case $MODE in
  0) DESCRIPTION="Brush size"
     RINGDOWN="key ["
     RINGUP="key ]" ;;
  1) DESCRIPTION="Scroll / Zoom"
     RINGDOWN="button 5"
     RINGUP="button 4" ;;
  2) DESCRIPTION="Switch layers"
     RINGDOWN="key PgDn"
     RINGUP="key PgUp" ;;
  3) DESCRIPTION="Rotate canvas"
     RINGDOWN="key ctrl ]"
     RINGUP="key ctrl [" ;;
  *) echo "Unknown mode $MODE"
     exit 1
     ;;
esac

xsetwacom set "$PAD" AbsWheelDown "$RINGDOWN"
xsetwacom set "$PAD" AbsWheelUp "$RINGUP"

if command -v notify-send > /dev/null 2>&1
then notify-send -a "Wacom Tablet" "Wacom Ring Mode" "$DESCRIPTION"
fi
