import Toybox.WatchUi;

class EnterRouteKeyView extends WatchUi.View {
  function initialize() {
    View.initialize();
  }

  function onLayout(dc as Toybox.Graphics.Dc) as Void {
    setLayout(Rez.Layouts.TodoLayout(dc));
  }

  function onShow() as Void {}

  function onHide() as Void {}

  function onUpdate(dc as Toybox.Graphics.Dc) as Void {
    View.onUpdate(dc);
  }
}

class EnterRouteKeyBehaviorDelegate extends WatchUi.BehaviorDelegate {
  function initialize() {
    BehaviorDelegate.initialize();
  }
}
