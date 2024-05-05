import Toybox.Lang;
import Toybox.Graphics;
import Toybox.WatchUi;
import Toybox.Math;

class MainActionMenuInputDelegate extends WatchUi.Menu2InputDelegate {
  function initialize() {
    Menu2InputDelegate.initialize();
  }

  function onSelect(item) {
    var itemId = item.getId();
    switch (itemId) {
      case :add_route:
        WatchUi.pushView(
          new NewRouteView(),
          new NewRouteBehaviorDelegate(),
          WatchUi.SLIDE_IMMEDIATE
        );
        break;

      case :route_key:
        WatchUi.pushView(
          new EnterRouteKeyView(),
          new EnterRouteKeyBehaviorDelegate(),
          WatchUi.SLIDE_IMMEDIATE
        );
        break;
    }
  }
}

class MainActionMenuView extends WatchUi.View {
  function initialize() {
    View.initialize();
  }

  function onLayout(dc as Dc) as Void {
    setLayout(Rez.Layouts.EmptyLayout(dc));
  }

  function onShow() as Void {
    var menu = new Rez.Menus.MainActionMenu();
    var delegate = new MainActionMenuInputDelegate();
    WatchUi.pushView(menu, delegate, WatchUi.SLIDE_IMMEDIATE);
  }

  function onUpdate(dc as Dc) as Void {
    View.onUpdate(dc);
  }

  function onHide() as Void {}
}
