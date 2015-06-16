var profiles = {
    'Krita': 'Krita',
    'gimp': 'Gimp'
};

function withTablet(fn) {
    callDBus("org.kde.Wacom", "/Tablet", "org.kde.Wacom", "getTabletList", callback=function(tablets) {
      if (tablets) {      
          fn(""+tablets);
      } 
    });
}

function setTabletProfile(tablet, newProfile) {
    callDBus("org.kde.Wacom", "/Tablet", "org.kde.Wacom", "setProfile", tablet, newProfile);
}


function setProfile(newProfile) {
  withTablet(function(tablet) {
    setTabletProfile(tablet, newProfile);
  });
}

function withTabletProfile(fn) {
  withTablet(function(tablet) {
      callDBus("org.kde.Wacom", "/Tablet", "org.kde.Wacom", "getProfile", tablet, callback=function(profile) {
          fn(tablet, profile);
      });
  });
}


function onFocus(client) {
    if (! client || typeof(client) == 'undefined') {
      return;
    }
    withTabletProfile(function(tablet, oldProfile) {
      var newProfile = profiles[client.resourceClass];
      if (newProfile == undefined) {
          setTabletProfile(tablet, 'Default');
      } else if (newProfile != oldProfile) 
          setTabletProfile(tablet, newProfile);
      }
    });
}

workspace.clientActivated.connect(onFocus);
