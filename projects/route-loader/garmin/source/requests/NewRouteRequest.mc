import Toybox.Lang;

using Toybox.Communications as Comm;
using Toybox.Application;
using Toybox.System;
using Toybox.WatchUi;

class RequestDelegate {
  hidden var _onSuccess as Method;

  function initialize(onSuccess as Method) {
    _onSuccess = onSuccess;
  }

  function handleResponse(data) {
    _onSuccess.invoke(data);
  }
}

function showErrorModal(statusCode, body) {
  var msg = "HTTP " + statusCode + "\n\n";
  if (body != null) {
    msg += body.toString();
  }

  WatchUi.pushView(
    new ModalMessageView("Network Error", msg, null),
    new WatchUi.BehaviorDelegate(),
    WatchUi.SLIDE_IMMEDIATE
  );
}

class Request {
  hidden var _delegate;

  function initialize(delegate) {
    _delegate = delegate;
  }

  function sendRequest() {}

  function onResponse(status as Lang.Number, data as Lang.Dictionary?) as Void {
    if (status == 200) {
      _delegate.handleResponse(data);
    } else {
      showErrorModal(status, data);
    }
  }
}

class NewRouteModel {
  var uploadURL as String;
  var downloadURL as String;

  function initialize(uploadURL as String, downloadURL as String) {
    self.uploadURL = uploadURL;
    self.downloadURL = downloadURL;
  }
}

class NewRouteRequestDelegate extends RequestDelegate {
  function initialize(callback) {
    RequestDelegate.initialize(callback);
  }

  function handleResponse(data as Toybox.Lang.Dictionary) {
    _onSuccess.invoke(
      new NewRouteModel(data["upload_url"], data["download_url"])
    );
  }
}

class NewRouteRequest extends Request {
  function initialize(delegate) {
    Request.initialize(delegate);
  }

  function sendRequest() {
    var baseURL = Toybox.Application.Properties.getValue("routeMirrorBaseURL");
    var url = baseURL + "/api/upload_url";

    Comm.makeWebRequest(
      url,
      {},
      {
        :method => Comm.HTTP_REQUEST_METHOD_GET,
        :responseType => Comm.HTTP_RESPONSE_CONTENT_TYPE_JSON,
      },
      method(:onResponse)
    );
  }
}

class DownloadGPXRequest extends Request {
  hidden var _url as String;

  function initialize(url as String, delegate) {
    _url = url;
    Request.initialize(delegate);
  }

  function sendRequest() {
    Comm.makeWebRequest(
      _url,
      {},
      {
        :method => Comm.HTTP_REQUEST_METHOD_GET,
        :responseType => Comm.HTTP_RESPONSE_CONTENT_TYPE_FIT,
      },
      method(:floorb)
    );
  }

  function floorb(
    statusCode as Toybox.Lang.Number,
    downloads as Toybox.PersistedContent.Iterator?
  ) as Void {
    System.println(statusCode);
    if (statusCode != 200) {
      showErrorModal(statusCode, downloads);
      return;
    }

    var download = downloads.next();
    System.println(
      "onReceiveTrack: " +
        (download == null ? null : download.getName() + "/" + download.getId())
    );

    if (download != null) {
      WatchUi.requestUpdate();
      System.exitTo(download.toIntent());
      return;
    }
  }
}
