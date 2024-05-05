import Toybox.Application;
import Toybox.Lang;
import Toybox.WatchUi;
import Toybox.Math;

using Toybox.System;

class RouteMirrorApp extends Application.AppBase {
  function getInitialView() as [Views] or [Views, InputDelegates] {
    if (!System.getDeviceSettings().phoneConnected) {
      return [new ConnectToGcmView()];
    }

    return [new MainActionMenuView()];
  }

  function initialize() {
    AppBase.initialize();
  }

  function onStart(state as Dictionary?) as Void {
    // onStart() is called on application start up
  }

  function onStop(state as Dictionary?) as Void {
    // onStop() is called when your application is exiting
  }
}

function getApp() as RouteMirrorApp {
  return Application.getApp() as RouteMirrorApp;
}
