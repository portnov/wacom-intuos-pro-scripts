#!/usr/bin/env python

import os
import sys
import re
from glob import glob
from subprocess import Popen, PIPE, STDOUT
import yaml
import argparse

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

def notify(text):
    if pynotify_available:
        try:
            n = pynotify.Notification("Wacom Tablet Ring Mode", text)
            n.set_timeout(2)
            n.show()
        except Exception:
            print(text)
    else:
        print(text)

def xsetwacom(args):
    command = "xsetwacom " + args
    print(command)
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

def set_mode(path, mode, mode_idx):
    pad = get_pad_devicename()
    if 'ring_down' in mode:
        assign_tablet_button(pad, 'AbsWheelDown', mode['ring_down'])
    if 'ring_up' in mode:
        assign_tablet_button(pad, 'AbsWheelUp', mode['ring_up'])
    with open(path, 'w') as f:
        f.write(str(mode_idx))

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

def parse_cmdline():
    parser = argparse.ArgumentParser(description="Toggle or set Wacom tablet's express ring operation mode.")
    parser.add_argument('-c', '--config', nargs=1, metavar='FILENAME', help='Use the specified config file')
    parser.add_argument('--reset', action='store_true', help='Reset ring mode to default for current tablet profile')
    return parser.parse_args()

def load_config(path=None):
    if path is None:
        path = os.path.expanduser("~/.config/wacom-ring-modes.yaml")
    config = yaml.safe_load(open(path,'r').read())
    return config

def toggle_mode(config, reset=False):
    path = get_control_file_path()
    if not path:
        print("Wacom tablet does not appear to be attached.")
        sys.exit(1)
    if not is_writeable(path):
        print("You have no write permission to Wacom LED control file {}.".format(path))
        sys.exit(2)

    profile = get_profile()
    config = load_config()
    if profile not in config:
        if 'Default' in config:
            print("Profile {} is not described in the config. Using default profile.".format(profile))
            profile = 'Default'
        else:
            print("Profile {} is not described in the config, and the Default profile is not described either.".format(profile))
            sys.exit(3)

    modes = config[profile]
    nmodes = len(modes)

    old_mode_idx = get_mode(path)
    old_mode = modes[old_mode_idx].get("description", str(old_mode_idx))
    if reset:
        new_mode_idx = 0
    else:
        new_mode_idx = (old_mode_idx + 1) % nmodes
    new_mode = modes[new_mode_idx].get("description", str(new_mode_idx))
    set_mode(path, modes[new_mode_idx], new_mode_idx)
    notify("Mode switched from {} to {}".format(old_mode, new_mode))

if __name__ == "__main__":

    args = parse_cmdline()
    print(args)
    config = load_config(args.config)
    toggle_mode(config, args.reset)

