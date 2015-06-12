#!/bin/bash -x

sudo cp wacom-setup.sh /usr/local/sbin/
sudo cp wacom-ring-mode.sh /usr/local/bin/
sudo cp wacom-switch-mode.sh /usr/local/bin/
sudo cp 99-local.rules /lib/udev/rules.d/
