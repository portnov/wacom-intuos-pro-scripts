#!/bin/bash

COUNT=10
FOUND=0
while [ $COUNT -gt 0 ]
do if [ -f /sys/bus/usb/devices/*/*/wacom_led/status_led0_select ]
   then FOUND=1
        break
   else echo "File does not exist yet, waiting" >> /var/log/wacom
        sleep 1s
   fi
   COUNT=$((COUNT-1))
done

if [ $FOUND = 1 ]
then chmod -v a+w /sys/bus/usb/devices/*/*/wacom_led/status_led0_select >> /var/log/wacom
else echo File was not found for 10 seconds, exiting >> /var/log/wacom
fi

