
function setProfile(newProfile) {
    callDBus("org.kde.Wacom", "/Tablet", "org.kde.Wacom", "getTabletList", callback=function(tablets) {
    if (tablets) {      
        callDBus("org.kde.Wacom", "/Tablet", "org.kde.Wacom", "setProfile", ""+tablets, newProfile);
    } else {
        print("No tablets detected");
    }
    });
}

var profiles = {
    'Krita': 'Krita',
    'gimp': 'Gimp'
};

function onFocus(client) {
    if (! client || typeof(client) == 'undefined') {
      return;
    }
    var found = false;
    for (cls in profiles) {
        if (client.resourceClass == cls) {
            found = true;
            setProfile(profiles[cls]);
            break;
        }
    }
    if (! found) {
      setProfile("Default");
    }
}

workspace.clientActivated.connect(onFocus);
