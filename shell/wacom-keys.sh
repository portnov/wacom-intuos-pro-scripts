#!/bin/bash

PAD=$(xsetwacom list dev | grep -o ".*pad")
STYLUS=$(xsetwacom list dev | grep -o ".*stylus")
TOUCH=$(xsetwacom list dev | grep -o ".*touch")

# 2
# 3
# 8
# 9
# (1)
# 10
# 11
# 12
# 13

xsetwacom set "$PAD" Button 2 "key ctrl"
xsetwacom set "$PAD" Button 3 "key shift"
xsetwacom set "$PAD" Button 8 "key alt"
xsetwacom set "$PAD" Button 9 "key ctrl z"

xsetwacom set "$PAD" Button 1 "button 3"

xsetwacom set "$PAD" Button 10 "key H"
xsetwacom set "$PAD" Button 11 "key L"
xsetwacom set "$PAD" Button 12 "key Delete"
xsetwacom set "$PAD" Button 13 "key B"

xsetwacom set "$TOUCH" Touch off

