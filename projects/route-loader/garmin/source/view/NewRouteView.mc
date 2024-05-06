import Toybox.WatchUi;

enum {
  NRW_INIT,
  NRW_FETCH_ROUTE_LINK,
  NRW_WAITING_FOR_USER,
}

class NewRouteView extends WatchUi.View {
  hidden var _state = NRW_INIT;
  hidden var _data as NewRouteModel?;

  function initialize() {
    View.initialize();
  }

  function onLayout(dc as Toybox.Graphics.Dc) as Void {
    setLayout(Rez.Layouts.NewRouteLayoutFetchLink(dc));
  }

  function onShow() as Void {
    if (_state == NRW_INIT) {
      _state = NRW_FETCH_ROUTE_LINK;
      var req = new NewRouteRequest(
        new NewRouteRequestDelegate(method(:onNewRouteRequestComplete))
      );
      req.sendRequest();
    }
  }

  function onHide() as Void {}

  function onUpdate(dc as Toybox.Graphics.Dc) as Void {
    View.onUpdate(dc);
  }

  function onNewRouteRequestComplete(data as NewRouteModel) {
    _state = NRW_WAITING_FOR_USER;
    _data = data;

    Communications.openWebPage(data.uploadURL, {}, null);

    WatchUi.pushView(
      new WatchUi.Confirmation("Ready?"),
      new FooBar(self, data.downloadURL),
      WatchUi.SLIDE_IMMEDIATE
    );
  }

  function setState(state) {
    _state = state;
    self.requestUpdate();
  }
}

class FooBar extends WatchUi.ConfirmationDelegate {
  hidden var _view;
  hidden var _url;

  function initialize(view, url) {
    _view = view;
    _url = url;

    ConfirmationDelegate.initialize();
  }

  function onComplete() {
    WatchUi.switchToView(
      new WatchUi.Confirmation("Done!"),
      new WatchUi.ConfirmationDelegate(),
      WatchUi.SLIDE_IMMEDIATE
    );
  }

  function onResponse(response) {
    if (response == WatchUi.CONFIRM_YES) {
      var req = new DownloadGPXRequest(_url, method(:onComplete));
      req.sendRequest();
    }

    _view.setState(NRW_WAITING_FOR_USER);
    return true;
  }
}

class NewRouteBehaviorDelegate extends WatchUi.BehaviorDelegate {
  function initialize() {
    BehaviorDelegate.initialize();
  }
}
