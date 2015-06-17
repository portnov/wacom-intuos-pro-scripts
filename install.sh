#!/bin/bash -x

sudo cp udev/wacom-setup.sh /usr/local/sbin/
sudo cp udev/99-local.rules /lib/udev/rules.d/

sudo cp python/ring-mode.py /usr/local/bin/
