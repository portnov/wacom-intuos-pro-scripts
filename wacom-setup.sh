#!/bin/sh

echo chmod a+w /sys/bus/usb/devices/*/*/wacom_led/status_led0_select >> /var/log/wacom
chmod a+w /sys/bus/usb/devices/*/*/wacom_led/status_led0_select

