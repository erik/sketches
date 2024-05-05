import Toybox.WatchUi;

class ConnectToGcmView extends WatchUi.View {
  function initialize() {
    View.initialize();
  }

  function onLayout(dc) {
    setLayout(Rez.Layouts.ConnectToGcmLayout(dc));
  }
}
