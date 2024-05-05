using Toybox.WatchUi;

class ErrorView extends WatchUi.View {
  hidden var _message;

  function initialize(message) {
    View.initialize();
    _message = message;
  }

  function onLayout(dc) {
    setLayout(Rez.Layouts.ErrorLayout(dc));

    var view = View.findDrawableById("Message") as Toybox.WatchUi.Text;
    view.setText(_message.toString());
  }
}
