#!/usr/bin/env python

import os
import sys
import re
from glob import glob
from subprocess import Popen, PIPE, STDOUT
import yaml

import gobject
gobject.threads_init()

from dbus import glib
glib.init_threads()

import dbus

try:
    import pynotify
    pynotify_available = True
    pynotify.init("Wacom Tablet")
except ImportError:
    print("Python libnotify bindings are not available")
    pynotify_available = False

CONTROL_PATH_MASK = "/sys/bus/usb/devices/*/*/wacom_led/status_led0_select"

def get_control_file_path():
    files = glob(CONTROL_PATH_MASK)
    if files:
        return files[0]
    else:
        return None

def is_writeable(path):
    return os.access(path, os.W_OK)

def get_mode(path):
    with open(path, 'r') as f:
        return int(f.read())

def set_mode(path, mode):
    with open(path, 'w') as f:
        f.write(str(mode))

def notify(text):
    if pynotify_available:
        pynotify.Notification("Wacom Tablet Ring Mode", text).show()
    else:
        print(text)

def xsetwacom(args):
    command = "xsetwacom " + args
    p = Popen(command, shell=True, stdin=PIPE, stdout=PIPE, close_fds=True)
    return p

def get_pad_devicename():
    r = re.compile(".*pad")
    p = xsetwacom("list dev")
    for line in iter(p.stdout.readline, ''):
        m = r.match(line)
        if m:
            return m.group()
    return None

def assign_tablet_button(pad, button, action):
    xsetwacom("set \"{}\" {} \"{}\"".format(pad, button, action))

def get_profile():
    # Create a session bus.
    bus = dbus.SessionBus()
    obj = bus.get_object('org.kde.Wacom', '/Tablet')
    iface = dbus.Interface(obj, 'org.kde.Wacom')
    try:
        tablets = iface.getTabletList()
        if tablets:
            profile = iface.getProfile(tablets[0])
        else:
            return None
    except dbus.DBusException:
        profile = iface.getProfile()
    return str(profile)


def toggle_mode(nmodes=4):
    path = get_control_file_path()
    if not path:
        print("Wacom tablet does not appear to be attached.")
        sys.exit(1)
    if not is_writeable(path):
        print("You have no write permission to Wacom LED control file {}.".format(path))
        sys.exit(2)

    old_mode = get_mode(path)
    new_mode = (old_mode + 1) % nmodes
    set_mode(path, new_mode)
    notify("Mode switched from {} to {}".format(old_mode, new_mode))

if __name__ == "__main__":

    toggle_mode()

