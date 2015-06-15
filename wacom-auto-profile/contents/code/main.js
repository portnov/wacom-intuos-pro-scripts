
callDBus("org.kde.Wacom", "/Tablet", "org.kde.Wacom", "getTabletList", callback=function(tablets) {
  if (tablets) {
