using Toybox.WatchUi;
import Toybox.Lang;

class ModalMessageView extends WatchUi.View {
  hidden var _title as String;
  hidden var _body as String;
  hidden var _button as String;

  function initialize(title as String, body as String, button as String?) {
    View.initialize();
    _title = title;
    _body = body;
    _button = button == null ? "Ok" : button;
  }

  function onLayout(dc) {
    setLayout(Rez.Layouts.ModalLayout(dc));

    var title = View.findDrawableById("title") as Toybox.WatchUi.Text;
    title.setText(_title);

    var body = View.findDrawableById("message") as Toybox.WatchUi.Text;
    body.setText(_body);

    var button = View.findDrawableById("button") as Toybox.WatchUi.Text;
    button.setText(_button);
  }
}
